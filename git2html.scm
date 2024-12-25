(module git2html ()

;; TODO
;; * list of commits (+ pagination)
;; * Check overwrite of files
;; * Handle symlinks
;; * Number lines (+ links to lines)

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
Usage: #prog -o <output-dir> <git-repo-dir>

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

(define (create-index dir-content git-dir output-dir #!key (preambule '()))
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

(define (create-preambule git-dir)
  (let ((repo-name (pathname-strip-directory (string-chomp git-dir "/"))))
    `((h1 (pre ,repo-name))
      (hr))))

(define (git-repo->html git-dir output-dir #!key link-parent?)
  (create-directory output-dir 'parents)
  (let ((dir-content (list-directory git-dir)))
    (create-index (if link-parent?
                      (cons ".." dir-content)
                      dir-content)
                  git-dir
                  output-dir
                  preambule: (if link-parent? '() (create-preambule git-dir)))
    (for-each
     (lambda (file)
       (let ((file-full-path (make-pathname git-dir file)))
         (if (directory? file-full-path)
             (let ((out-dir (make-pathname output-dir file)))
               (create-directory out-dir)
               (git-repo->html file-full-path out-dir link-parent?: #t))
             (with-output-to-file (make-pathname output-dir file "html")
               (lambda ()
                 (display
                  (html-page
                   `(pre
                     ,(with-input-from-file file-full-path read-string))
                   title: file)))))))
     dir-content)))

(let ((git-dir #f)
      (output-dir #f))
  (let loop ((args (command-line-arguments)))
    (unless (null? args)
      (let ((arg (car args)))
        (cond ((string=? arg "-o")
               (when (null? (cdr args))
                 (die! "-o: missing argument"))
               (set! output-dir (cadr args))
               (loop (cddr args)))
              (else
               (when git-dir
                 (usage 2))
               (set! git-dir arg)
               (loop (cdr args)))))))
  (unless (and output-dir git-dir)
    (usage 2))

  (git-repo->html git-dir output-dir)
  )

) ;; end module
