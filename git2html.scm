(module git2html ()

;; TODO
;; * create-preambule for files and commits
;; * Number lines (+ links to lines)
;; * Paginate commits
;; * Hard-link commits in different branches
;; * Check overwrite of files
;; * Handle symlinks

;; FIXME
(import big-chicken)
(import sxml-transforms srfi-1)

(import (chicken format)
        (chicken pathname)
        (chicken process-context))

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
  (let ((files (directory dir)))
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

(define (create-preambule git-dir #!key branch path)
  (let ((repo-name (pathname-strip-directory (string-chomp git-dir "/"))))
    `((h1 (code ,repo-name)
          ,(if branch
               `((literal "&nbsp") (code ,(sprintf "(~a)" branch)))
               '())
          ,(if path
               `((literal "&nbsp") (code ,path))
               '()))
      (hr))))

(define (string-prefix? maybe-substring string)
  (let ((pos (substring-index maybe-substring string)))
    (and pos (fx= 0 pos))))

(define (pathname-relative-to shortest-path longest-path)
  (let ((shortest-path (normalize-pathname shortest-path))
        (longest-path (normalize-pathname longest-path)))
    (if (string-prefix? shortest-path longest-path)
         (substring longest-path
                    (string-length shortest-path))
        (error 'pathname-relative-to
               (sprintf "~a is not relative to ~a" shortest-path longest-path)))))

(define (repo-files->html top-git-dir output-dir #!key link-parent? branch)

  (define (render-listing git-dir out-dir #!key (link-parent? #t))

    (define (%create-preambule path)
      (create-preambule top-git-dir
                        branch: branch
                        path: (pathname-relative-to path out-dir)))

    (let ((dir-content (list-directory git-dir)))
      (create-file-index
       (if link-parent?
           (cons ".." dir-content)
           dir-content)
       git-dir
       out-dir
       preambule: (%create-preambule
                   (make-pathname (list output-dir branch) "files")))
      (for-each
       (lambda (file)
         (let ((file-full-path (make-pathname git-dir file)))
           (if (directory? file-full-path)
               (let ((out-dir (make-pathname out-dir file)))
                 (create-directory out-dir)
                 (render-listing file-full-path out-dir))
               (with-output-to-file (make-pathname out-dir file "html")
                 (lambda ()
                   (display
                    (html-page
                     `(;; ,(%create-preambule (make-pathname (list output-dir branch "files") file))
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
    (render-listing top-git-dir out-dir link-parent?: #f)))

(define (create-project-index git-dir branches output-dir)
  (create-directory output-dir 'parents)
  (with-output-to-file (make-pathname output-dir "index.html")
    (lambda ()
      (display
       (html-page
        `(,(create-preambule git-dir)
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
          `(,(create-preambule git-dir branch: branch)
            (ul
             (li (a (@ (href "files")) "files"))
             (li (a (@ (href "commits")) "commits"))))))))))

(define (repo-commits->html git-dir output-dir #!key branch)
  (let ((max-log-lines 50)
        (log '())
        (out-dir (make-pathname
                    (if branch
                        (list output-dir branch)
                        output-dir)
                    "commits")))
    (with-input-from-pipe
        (sprintf "git -C ~a log --pretty='format:%H%x09%an%x09%s' | head -n ~a"
                 (qs git-dir)
                 ;; Little hack to detect whether there are more
                 ;; commits than what we are gonna show
                 (+ 1 max-log-lines))
      (lambda ()
        (let loop ()
          (let ((line (read-line)))
            (unless (eof-object? line)
              (set! log (cons line log))
              (loop))))))

    (create-directory out-dir 'parents)
    (let ((html-log '()))
      (for-each (lambda (line)
                  (let* ((tokens (string-split line "\t"))
                         (hash (car tokens))
                         (author (cadr tokens))
                         (subject (caddr tokens))
                         (commit (with-input-from-pipe (sprintf "git -C ~a show ~a"
                                                                (qs git-dir)
                                                                (qs hash))
                                   read-string)))
                    (set! html-log (cons `(tr
                                           (td (a (@ (href ,(make-pathname #f hash "html")))
                                                  ,hash))
                                           (td ,author)
                                           (td ,subject))
                                         html-log))
                    (with-output-to-file (make-pathname out-dir hash "html")
                      (lambda ()
                        (display
                         (html-page
                          `(pre ,commit)))))))
                log)
      (with-output-to-file (make-pathname out-dir "index.html")
        (lambda ()
          (display
           (html-page
            `(,(create-preambule git-dir branch: branch)
              (table ,(butlast html-log))
              ,(if (> (length html-log) max-log-lines)
                   `(p "The history displayed here is incomplete. "
                       "Check out the repository to see the full history.")
                   '()))
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
