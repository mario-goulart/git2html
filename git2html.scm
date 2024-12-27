(module git2html ()

;; TODO
;; * Breadcrumbs for paths
;; * Number lines (+ links to lines)
;; * Hard-link commits in different branches
;; * Check overwrite of files
;; * Handle symlinks
;; * parse configuration file
;; * Support svn
;; * Test bare repo

(import scheme)
(import (chicken base)
        (chicken file)
        (chicken file posix)
        (chicken fixnum)
        (chicken format)
        (chicken io)
        (chicken pathname)
        (chicken port)
        (chicken process)
        (chicken process-context)
        (chicken sort)
        (chicken string))
(import sxml-transforms srfi-1)

(define (usage #!optional exit-code)
  (let* ((port (if (and exit-code (not (zero? exit-code)))
                   (current-error-port)
                   (current-output-port)))
         (prog (pathname-strip-directory (program-name)))
         (msg #<#EOF
Usage: #prog -o <output-dir> [-b <branch>]  <git-repo-dir>

EOF
))
    (fprintf port msg)
    (when exit-code (exit exit-code))))

(define (die! fmt . args)
  (apply fprintf (cons (current-error-port)
                       (cons (string-append fmt "\n")
                             args)))
  (exit 1))


(define sxml->html
  (let ((rules `((literal *preorder* . ,(lambda (t b) b))
                 . ,universal-conversion-rules*)))
    (lambda (sxml)
      (with-output-to-string
        (lambda ()
          (SRV:send-reply (pre-post-order* sxml rules)))))))

(define (html-page content #!key (title ""))
  (sxml->html
   `((literal "<!DOCTYPE html>")
     (html
      (head
       (style "body { font-family: monospace, monospace; }")
       (title ,title))
      (body
       ,content)))))

(define (create-file-index dir-content git-dir output-dir #!key (preambule '()))
  (with-output-to-file (make-pathname output-dir "index.html")
    (lambda ()
      (display
       (sxml->html
        `(,preambule
          (ul
           ,@(map (lambda (file)
                    (let ((dir? (directory? (make-pathname git-dir file))))
                      `(li (a (@ (href ,(make-pathname #f file (if dir? #f "html"))))
                              ,(if dir?
                                   (string-append file "/")
                                   file)))))
                  dir-content))))))))

(define (list-directory dir)
  (let ((files (delete ".git" (directory dir 'dotfiles) string=?)))
    ;; Directories first
    (append (sort (filter (lambda (file)
                            (directory? (make-pathname dir file)))
                          files)
                  string<=?)
            (sort
             (remove (lambda (file)
                       (directory? (make-pathname dir file)))
                     files)
             string<=?))))

(define (depth->relative-path depth path)
  (let loop ((depth depth))
    (if (zero? depth)
        path
        (make-pathname ".." (loop (sub1 depth))))))

(define (create-preambule git-dir depth #!key branch path)
  (let ((repo-name (pathname-strip-directory (string-chomp git-dir "/"))))
    `((p (a (@ (href ,(depth->relative-path depth ""))) ,repo-name)
         ,(if branch
              `((literal "&nbsp")
                "("
                (a (@ (href ,(depth->relative-path depth branch))) ,branch)
                ")")
              '())
         ,(if path
              `((literal "&nbsp") ,path)
              '()))
      (hr))))

(define (string-prefix? maybe-substring string)
  (let ((pos (substring-index maybe-substring string)))
    (and pos (fx= 0 pos))))


(define (repo-files->html top-git-dir output-dir #!key link-parent? branch)

  (define (render-listing git-dir out-dir relpath depth #!key (link-parent? #t))
    (let* ((dir-content (list-directory git-dir))
           (%create-preambule
            (lambda (path)
              (create-preambule top-git-dir
                                (+ 2 depth) ;; +2 is for <branch>/files
                                branch: branch
                                path: path))))
      (create-file-index
       (if link-parent?
           (cons ".." dir-content)
           dir-content)
       git-dir
       out-dir
       preambule: (%create-preambule relpath))
      (for-each
       (lambda (file)
         (let ((file-full-path (make-pathname git-dir file)))
           (if (directory? file-full-path)
               (let ((out-dir (make-pathname out-dir file)))
                 (create-directory out-dir)
                 (render-listing file-full-path
                                 out-dir
                                 (make-pathname relpath file)
                                 (add1 depth)))
               (with-output-to-file (make-pathname out-dir file "html")
                 (lambda ()
                   (display
                    (html-page
                     `(,(%create-preambule (make-pathname relpath file))
                       (pre
                        ,(with-input-from-file file-full-path read-string)))
                     title: file)))))))
       dir-content)))

  (let ((out-dir (make-pathname
                  (if branch
                      (list output-dir branch)
                      output-dir)
                  "files")))
    (create-directory out-dir 'parents)
    (render-listing top-git-dir out-dir "/" 0 link-parent?: #f)))

(define (create-project-index git-dir branches output-dir)
  (create-directory output-dir 'parents)
  (with-output-to-file (make-pathname output-dir "index.html")
    (lambda ()
      (display
       (html-page
        `(,(create-preambule git-dir 0)
          (table
           ,@(map (lambda (branch)
                    `(tr
                      (td (bold ,branch))
                      (td (a (@ (href ,(make-pathname branch "files"))) "files"))
                      (td (a (@ (href ,(make-pathname branch "commits"))) "commits"))))
                  branches))))))))

(define (create-branch-index git-dir branch output-dir)
  (let ((branch-dir (make-pathname output-dir branch)))
    (create-directory branch-dir 'parents)
    (with-output-to-file (make-pathname branch-dir "index.html")
      (lambda ()
        (display
         (html-page
          `(,(create-preambule git-dir 1 branch: branch)
            (ul
             (li (a (@ (href "files")) "files"))
             (li (a (@ (href "commits")) "commits"))))))))))

(define (repo-commits->html git-dir output-dir #!key branch)
  (let ((log '())
        (out-dir (make-pathname
                    (if branch
                        (list output-dir branch)
                        output-dir)
                    "commits")))
    (with-input-from-pipe
        (sprintf "git -C ~a log --pretty='format:%H%x09%an%x09%s'" (qs git-dir))
      (lambda ()
        (let loop ()
          (let ((line (read-line)))
            (unless (eof-object? line)
              (set! log (cons line log))
              (loop))))))

    (create-directory out-dir 'parents)
    (let ((html-log '()))
      (for-each
       (lambda (line)
         (let* ((tokens (string-split line "\t"))
                (hash (car tokens))
                (author (cadr tokens))
                (subject (caddr tokens))
                (commit-file (make-pathname out-dir hash "html")))
           (set! html-log (cons `(tr
                                  (td (a (@ (href ,commit-file))
                                         ,hash))
                                  (td ,author)
                                  (td ,subject))
                                html-log))
           (unless (file-exists? commit-file)
             (let ((commit
                    (with-input-from-pipe
                        (sprintf "git -C ~a show --format=fuller ~a"
                                 (qs git-dir)
                                 (qs hash))
                      read-string)))
               (with-output-to-file commit-file
                 (lambda ()
                   (display
                    (html-page
                     `(pre ,commit)))))))))
       log)
      (with-output-to-file (make-pathname out-dir "index.html")
        (lambda ()
          (display
           (html-page
            `(,(create-preambule git-dir 2 ;; +2 is for <branch>/files
                                 branch: branch)
              (table ,(butlast html-log)))
            )))))))

(let ((git-dir #f)
      (output-dir #f)
      (branches '()))
  (let loop ((args (command-line-arguments)))
    (unless (null? args)
      (let ((arg (car args)))
        (cond ((string=? arg "-o")
               (when (null? (cdr args))
                 (die! "-o: missing argument"))
               (set! output-dir (cadr args))
               (loop (cddr args)))
              ((string=? arg "-b")
               (when (null? (cdr args))
                 (die! "-b: missing argument"))
               (set! branches (cons (cadr args) branches))
               (loop (cddr args)))
              (else
               (when git-dir
                 (usage 2))
               (set! git-dir arg)
               (loop (cdr args)))))))
  (unless (and output-dir git-dir)
    (usage 2))

  (if (null? branches)
      (repo-files->html git-dir output-dir)
      (begin
        (create-project-index git-dir branches output-dir)
        (for-each (lambda (branch)
                    (create-branch-index git-dir branch output-dir)
                    (repo-files->html git-dir output-dir branch: branch)
                    (repo-commits->html git-dir output-dir branch: branch))
                  branches)))
  )

) ;; end module
