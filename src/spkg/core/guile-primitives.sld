(define-library (spkg core guile-primitives)
  (import 
    (scheme base)
    (only (scheme file) file-exists?)
    (guile)
    (ice-9 pretty-print)
    (prefix (srfi 13) srfi-13))
  (export 
    pretty-print
    dirname 
    file-exists
    system
    getcwd
    canonicalize-path-string
    absolute-path-string?
    directory-list
    create-directory
    delete-file
    (rename file-is-directory? file-directory?)
    file-symbolic-link?
    change-file-mode
    rename-file
    create-symbolic-link
    create-hard-link
    file-stat-atime
    file-stat-mtime
    file-stat-ctime
    current-directory
    call-with-process-io
    process-wait)

  (syntax-violation #f "Guile is not yet supported; help on implementing guile-primitives.sld is welcome!")
  (define (directory-list dir)
    (define stream (opendir dir))
    (let loop ((entry (readdir stream)) (entries '()))
      (if (eof-object? entry)
          (begin
            (closedir stream)
            (reverse entries))
          (loop (readdir stream) (cons entry entries)))))
  (define (canonicalize-path-string path)
    (canonicalize-path path))
  (define (absolute-path-string? path)
    (absolute-path? path))

  (define (file-stat-atime path)
    (define st (stat path))
    (stat:atime st))
  (define (file-stat-mtime path)
    (define st (stat path))
    (stat:mtime st))
  (define (file-stat-ctime path)
    (define st (stat path))
    (stat:ctime st))

  (define (current-directory . optional-path)
    (if (null? optional-path)
        (getcwd)
        (chdir (car optional-path))))
  
  (define (string->command+args s)
    (let* ((s (if (string? s) s (format #f "~a" s)))
           (tokens (filter (lambda (x) (not (string-null? x)))
                           (string-split s #\space))))
      (if (null? tokens)
          (error "No command found in string" s)
          (values (car tokens) tokens))))

  (define (call-with-process-io command proc)
    (define input+output (pipe))
    (define-values (command args) (string->command+args command))
    (define pid (spawn command args #:input (car input+output)
                             #:output (cdr input+output)))

    (proc pid (car input+output) (cdr input+output) (cdr input+output)))
  
  (define (process-wait pid)
    (waitpid pid))

)