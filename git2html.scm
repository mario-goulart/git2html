(module git2html ()

(import scheme)
(import (chicken base)
        (chicken bitwise)
        (chicken condition)
        (chicken errno)
        (chicken file)
        (chicken file posix)
        (chicken fixnum)
        (chicken format)
        (chicken io)
        (chicken irregex)
        (chicken pathname)
        (chicken port)
        (chicken process)
        (chicken process signal)
        (chicken process-context)
        (chicken sort)
        (chicken string)
        (chicken time))
(import dot-locking srfi-1 srfi-13 sxml-transforms)

;; Will be set to #t if -link-repos-home is given on the command line
(define *link-repos-home?* #f)

;; Will be set to the directory name of the repo given on the command line
(define *repo-name* #f)

;; Will be set to the contents of the repo configuration file, if it exists
(define *conf* '())

;; Will be set to the path to the lock file once the output directory
;; is determined
(define *lock-file* #f)

;; If -trap-url is provided on the command line, will be set to the
;; URL used as argument.
(define *trap-url* #f)

;; Filenames that match the patterns below won't be converted to HTML.
;; Can be overriden by the `no-htmlize-patterns' option in the
;; repo-specific configuration.
(define +no-htmlize-patterns+
  (map (lambda (extension)
         (irregex `(: any "." ,extension eol) 'case-insensitive))
       '("bmp"
         "exe"
         "dll"
         "ico"
         "gif"
         "jpg" "jpeg"
         "mp3" "mp4"
         "o" "ogg"
         "pbm" "pdf" "png"
         "so"
         "wav" "webm"
         "xpm")))

(define (usage #!optional exit-code)
  (let* ((port (if (and exit-code (not (zero? exit-code)))
                   (current-error-port)
                   (current-output-port)))
         (prog (pathname-strip-directory (program-name)))
         (msg #<#EOF
Usage: #prog [<options>] <git-repo-dir> <output-dir>

<options>:

-b|-branch <branch>
    Branch to generate HTML files for. May be provided multiple times.

-f|-force-regenerate
    Force the regeneration of HTML files for commits.

-link-repos-home
    Add link to the parent directory of the repo directory (useful for
    combining multiple repositories).

-trap-url <URL>
    URL (or path) to be used as trap for evil crawlers.  Normally it should
    point to a location forbidden by robots.txt, so that requests can be
    distinguished from human users'.

EOF
))
;;| This is to prevent Emacs' syntax highlighter from screwing up
    (fprintf port msg)
    (when exit-code (exit exit-code))))

(define (with-environment-variable var val thunk)
  ;; This is not thread-safe!
  (let ((old-val (get-environment-variable var)))
    (dynamic-wind
        void
        (lambda ()
          (set-environment-variable! var val)
          (thunk))
        (lambda ()
          (if old-val
              (set-environment-variable! var old-val)
              (unset-environment-variable! var))))))

(define (die! fmt . args)
  (apply fprintf (cons (current-error-port)
                       (cons (string-append fmt "\n")
                             args)))
  (exit 1))

(define (copy-file-from-git top-git-dir branch git-file output-file)
  (system* (sprintf "git -C ~a show ~a:~a > ~a"
                    (qs top-git-dir)
                    (qs branch)
                    (qs git-file)
                    (qs output-file))))

(define (run-git args reader)
  (let* ((cmd (sprintf "git ~a" args))
         (p (open-input-pipe cmd))
         (out (with-input-from-port p reader))
         (exit-code (arithmetic-shift (close-input-pipe p) -8)))
    (unless (zero? exit-code)
      (die! "'~a' exited ~a" cmd exit-code))
    out))

(define sxml->html
  (let ((rules `((literal *preorder* . ,(lambda (t b) b))
                 . ,universal-conversion-rules*)))
    (lambda (sxml)
      (with-output-to-string
        (lambda ()
          (SRV:send-reply (pre-post-order* sxml rules)))))))

(define (html-page content title)
  (sxml->html
   `((literal "<!DOCTYPE html>")
     (html
      (head
       (meta (@ (charset "UTF-8")))
       (meta (@ (name "viewport")
                (content "width=device-width,initial-scale=1")))
       (style "\
body { font-family: monospace, monospace; }
table td { white-space: nowrap; }
td, th { padding: 2px; padding-right: 10px; }
pre.code { overflow: auto; }
pre.code a { color: #ccc; padding-right: 1ch; text-decoration: none; }
#trap { display: none; }
")
       (title ,title))
      (body
       ,(list content
              (if *trap-url*
                  `(a (@ (href ,*trap-url*)
                         (id "trap"))
                      "Trap")
                  '())))))))

(define (conf-ref key #!key default)
  (alist-ref key *conf* eq? default))

(define (write-html-page file sxml #!key (title ""))
  (with-output-to-file file
    (lambda ()
      (display
       (html-page sxml title)))))

(define (page-title title)
  (sprintf "~a - ~a" title *repo-name*))

(define (sort-files abs-dir)
  ;; Directories first
  (let* ((files (directory abs-dir))
         (sorted
          (append (sort (filter (lambda (file)
                                  (directory? (make-pathname abs-dir file)))
                                files)
                        string<=?)
                  (sort
                   (remove (lambda (file)
                             (directory? (make-pathname abs-dir file)))
                           files)
                   string<=?))))
    (map pathname-strip-directory sorted)))

(define (depth->relative-path depth path)
  ;; Create a relative path based on the depth of `path' in a certain
  ;; directory.  Example:
  ;;   (depth->relative-path 2 "foo") => "../../foo"
  (let loop ((depth depth))
    (if (zero? depth)
        path
        (make-pathname ".." (loop (sub1 depth))))))

(define (create-preamble git-dir depth #!key branch path)
  `((p
     ,(if *link-repos-home?*
          `((a (@ (href ,(depth->relative-path depth ".."))) "~")
            " ")
          '())
     (a (@ (href ,(depth->relative-path depth ""))) ,*repo-name*)
     ,(if branch
          `((literal "&nbsp;")
            "("
            (a (@ (href ,(depth->relative-path depth branch))) ,branch)
            ")")
          '())
     ,(if path
          `((literal "&nbsp;") ,path)
          '()))
    (hr)))

(define (read-git-file top-git-dir file branch)
  (run-git (sprintf "-C ~a show ~a:~a"
                    (qs top-git-dir)
                    (qs branch)
                    (qs file))
           read-lines))

(define (num-digits n)
  ;; Return the number of digits in `n' (a positive integer).
  (if (zero? n)
      1
      (inexact->exact (floor (add1 (log n 10))))))

(define (pad-lineno lineno max-digits)
  ;; Left-pad lineno with (literal "&nbsp;") to have proper alignement
  ;; of digits.
  (let ((lineno-num-digits (num-digits lineno)))
    (let loop ((max-digits max-digits))
      (if (= lineno-num-digits max-digits)
          (list lineno)
          (cons '(literal "&nbsp;") (loop (sub1 max-digits)))))))

(define (enumerate-lines lines)
  (let ((max-digits (num-digits (length lines))))
    `(pre (@ (class "code"))
          ,@(map (lambda (line lineno)
                   `(code (@ (id ,(sprintf "L~a" lineno)))
                          (a (@ (href ,(sprintf "#L~a" lineno)))
                             ,(pad-lineno lineno max-digits))
                          ,(string-append line "\n")))
                 lines
                 (iota (length lines) 1)))))

(define (list-git-repo top-git-dir branch)
  (run-git (sprintf "-C ~a ls-tree --name-only --full-tree -r ~a"
                    (qs top-git-dir)
                    (qs branch))
           read-lines))

(define (htmlize-file? path)
  ;; path is relative to the root of the git repository directory
  (let loop ((patterns (conf-ref 'no-htmlize-patterns
                                 default: +no-htmlize-patterns+)))
    (if (null? patterns)
        #t
        (let ((pattern (car patterns)))
          (if (irregex-search pattern path)
              #f
              (loop (cdr patterns)))))))

(define (git-repo-files->html top-git-dir output-dir branch)
  (let ((out-dir (make-pathname (list output-dir branch) "files"))
        (listing (list-git-repo top-git-dir branch)))
    ;; Even in incremental mode we have to remove the directory
    ;; containing files, as commits might have removed them, in which
    ;; case they would still be displayed if not removed.
    (handle-exceptions exn
      (unless (eq? (get-condition-property exn 'exn 'errno) errno/noent)
        (signal exn))
      (delete-directory out-dir 'recursively))
    (create-directory out-dir 'parents)

    ;; Create files
    (for-each
     (lambda (file)
       (if (htmlize-file? file)
           (let* ((rel-dir (pathname-directory file))
                  (depth (string-count file #\/)))
             (create-directory (make-pathname out-dir rel-dir) 'parents)
             (write-html-page (make-pathname out-dir file "html")
               `(,(create-preamble top-git-dir
                                   (+ 2 depth) ;; +2 is for <branch>/files
                                   branch: branch
                                   path: (make-absolute-pathname #f file))
                 ,(enumerate-lines (read-git-file top-git-dir file branch)))
               title: (page-title file)))
           (copy-file-from-git
            top-git-dir branch file (make-pathname out-dir file))))
     listing)

    ;; Create index.html files for directory listings
    (let ((dirs (cons out-dir (find-files out-dir test: directory?))))
      (for-each
       (lambda (dir)
         (let* ((rel-dir (substring dir (string-length (string-chomp out-dir "/"))))
                (depth (string-count rel-dir #\/))
                (dir-content (if (zero? depth)
                                 (sort-files dir)
                                 (cons ".." (sort-files dir)))))
           (write-html-page (make-pathname dir "index.html")
             `(,(create-preamble top-git-dir
                                 (+ 2 depth)  ;; +2 is for <branch>/files
                                 branch: branch
                                 path: (make-absolute-pathname #f rel-dir))
               (ul
                ,@(map (lambda (file)
                         (let ((dir? (directory? (make-pathname dir file)))
                               (get-filename
                                (lambda (file)
                                  (if (string-suffix? ".html" file)
                                      (pathname-file file)
                                      file))))
                           `(li (a (@ (href ,(make-pathname
                                              #f
                                              (get-filename file)
                                              (if (or dir?
                                                      (not (string-suffix? ".html" file)))
                                                  #f
                                                  "html"))))
                                   ,(if dir?
                                        (string-append file "/")
                                        (get-filename file))))))
                       dir-content)))
             title: (page-title (make-absolute-pathname #f rel-dir)))))
       dirs))))

(define (create-project-index git-dir branches output-dir)
  (create-directory output-dir 'parents)
  (write-html-page (make-pathname output-dir "index.html")
    `(,(create-preamble git-dir 0)
      ;; Description
      ,(let ((description (conf-ref 'description)))
         (if description
             `((h2 "Description")
               ,description)
             '()))
      ;; Checkout instructions
      ,(let ((checkout-instructions (conf-ref 'checkout-instructions)))
         (if checkout-instructions
             `((h2 "Checkout instructions")
               ,checkout-instructions)
             '()))
      (h2 "Branches")
      (table
       ,@(map (lambda (branch)
                `(tr
                  (td (bold ,branch))
                  (td (a (@ (href ,(make-pathname branch "files")))
                         "files"))
                  (td (a (@ (href ,(make-pathname branch "commits")))
                         "commits"))))
              branches)))
    title: *repo-name*))

(define (create-branch-index git-dir branch output-dir)
  (let ((branch-dir (make-pathname output-dir branch)))
    (create-directory branch-dir 'parents)
    (write-html-page (make-pathname branch-dir "index.html")
      `(,(create-preamble git-dir 1 branch: branch)
        (ul
         (li (a (@ (href "files")) "files"))
         (li (a (@ (href "commits")) "commits"))))
      title: (page-title branch))))

(define (git-repo-commits->html git-dir output-dir #!key branch force-regenerate)
  (let ((log '())
        ;; Directory for all commits -- branch-specific commits will
        ;; link to files here.
        (commits-dir (make-pathname (list output-dir ".git") "commits"))
        (branch-commits-dir (make-pathname
                             (if branch
                                 (list output-dir branch)
                                 output-dir)
                             "commits")))
    (create-directory commits-dir 'parents)
    (run-git (sprintf "-C ~a log --pretty='format:%H%x09%an%x09%s'"
                      (qs git-dir))
             (lambda ()
               (let loop ()
                 (let ((line (read-line)))
                   (unless (eof-object? line)
                     (set! log (cons line log))
                     (loop))))))
    (create-directory branch-commits-dir 'parents)
    (let ((html-log '()))
      (for-each
       (lambda (line)
         (let* ((tokens (string-split line "\t"))
                (hash (car tokens))
                (author (cadr tokens))
                (subject (caddr tokens))
                (commit-file (make-pathname commits-dir hash "html"))
                (branch-commit-file (make-pathname branch-commits-dir hash "html"))
                (web-commit-file (make-pathname #f hash "html")))
           (set! html-log (cons `(tr
                                  (td (a (@ (href ,web-commit-file))
                                         ,hash))
                                  (td ,author)
                                  (td ,subject))
                                html-log))
           (when (or force-regenerate (not (file-exists? commit-file)))
             (let ((commit
                    (run-git (sprintf "-C ~a show --format=fuller ~a"
                                      (qs git-dir)
                                      (qs hash))
                             read-string)))
               (write-html-page commit-file
                 `(,(create-preamble git-dir 2 ;; +2 is for <branch>/files
                                     path: hash
                                     branch: branch)
                   (pre ,commit))
                 title: (page-title hash))))
           (handle-exceptions exn
             (unless (eq? (get-condition-property exn 'exn 'errno) errno/exist)
               (signal exn))
             (file-link commit-file branch-commit-file))))
       log)
      ;; Table of branch commits
      (write-html-page (make-pathname branch-commits-dir "index.html")
        `(,(create-preamble git-dir 2 ;; +2 is for <branch>/files
                            branch: branch)
          (table ,(butlast html-log)))
        title: (page-title "commits")))))

(define (main args)
  (let ((git-dir #f)
        (output-dir #f)
        (branches '())
        (force-regenerate #f))
    (let loop ((args args))
      (unless (null? args)
        (let ((arg (car args)))
          (cond ((member arg '("-h" "-help" "--help"))
                 (usage 0))
                ((member arg '("-b" "-branch"))
                 (when (null? (cdr args))
                   (die! "-b: missing argument"))
                 (set! branches (cons (cadr args) branches))
                 (loop (cddr args)))
                ((member arg '("-f" "-force-regenerate"))
                 (set! force-regenerate #t)
                 (loop (cdr args)))
                ((string=? arg "-link-repos-home")
                 (set! *link-repos-home?* #t)
                 (loop (cdr args)))
                ((string=? arg "-trap-url")
                 (when (null? (cdr args))
                   (die! "-trap-url: missing argument"))
                 (set! *trap-url* (cadr args))
                 (loop (cddr args)))
                (else
                 (cond ((and git-dir output-dir)
                        (die! "Invalid option: ~a" arg))
                       ((and (not git-dir) (not output-dir))
                        (set! git-dir arg))
                       ((not output-dir)
                        (set! output-dir arg)))
                 (loop (cdr args)))))))

    (unless (and output-dir git-dir)
      (usage 2))

    (let ((lock-dir (make-pathname output-dir ".git")))
      (create-directory lock-dir 'parents)
      (set! *lock-file* (make-pathname lock-dir ".git2html"))
      ;; dot-locking creates a temporary file to hard-link to the
      ;; actual lock file.  To reduce the chances that the temporary
      ;; file and the lock file end up in different filesystems (in
      ;; which case hard-linking would not be possible), we set TMPDIR
      ;; (used by create-directory) to the lock directory.
      (with-environment-variable "TMPDIR" lock-dir
         (lambda ()
           (obtain-dot-lock *lock-file* 10 15 3600))))

    (set! *repo-name*
          (pathname-file (string-chomp (normalize-pathname git-dir) "/")))

    ;; Read repo configuration file
    (handle-exceptions exn
      (unless (eq? (get-condition-property exn 'exn 'errno) errno/noent)
        (signal exn))
      (set! *conf*
            (with-input-from-file (make-pathname git-dir ".git2html.scm")
              read-list)))

    (let ((branches (if (null? branches)
                        (or (conf-ref 'branches)
                            '("master"))
                        branches)))
      (create-project-index git-dir branches output-dir)
      (for-each (lambda (branch)
                  (create-branch-index git-dir branch output-dir)
                  (git-repo-files->html git-dir output-dir branch)
                  (git-repo-commits->html git-dir output-dir
                                          branch: branch
                                          force-regenerate: force-regenerate))
                branches))))

(on-exit
 (lambda ()
   (when *lock-file*
     (release-dot-lock *lock-file*))))

(set-signal-handler! signal/int
  (lambda (signal)
    ;; This is just to force a call to exit on C-c, so that the lock
    ;; file gets released.
    (exit 2)))

(handle-exceptions exn
  (begin
    (print-call-chain)
    (print-error-message exn)
    (exit 1))
  (main (command-line-arguments)))

) ;; end module
