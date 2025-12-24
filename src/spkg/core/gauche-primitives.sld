(define-library (spkg core gauche-primitives)
  (import 
    (scheme base)
    (only (scheme file) file-exists?)
    (gauche base) 
    (only (file util)
      directory-list
      current-directory
      create-directory*
      delete-file)
    (only (srfi 170)
      file-info-symlink?)
    (gauche process))
  (export 
    pretty-print
    (rename sys-dirname dirname)
    file-exists?
    (rename sys-system system)
    (rename sys-getcwd getcwd)
    canonicalize-path-string
    (rename absolute-path? absolute-path-string?)
    directory-list
    (rename create-directory* create-directory)
    delete-file
    (rename file-is-directory? file-directory?)
    (rename file-info-symlink? file-symbolic-link?)
    (rename sys-chmod change-file-mode)
    (rename sys-rename rename-file)
    (rename sys-symlink create-symbolic-link)
    (rename sys-link create-hard-link)
    file-stat-atime
    file-stat-mtime
    file-stat-ctime
    current-directory
    (rename %call-with-process-io call-with-process-io)
    process-wait)
    
(begin 
  (define (canonicalize-path-string path)
    (sys-normalize-pathname path :canonicalize #t :absolute #t))
  (define (file-stat-atime path)
    (define st (sys-stat path))
    (~ stat'atim))
  (define (file-stat-mtime path)
    (define st (sys-stat path))
    (~ stat'mtim))
  (define (file-stat-ctime path)
    (define st (sys-stat path))
    (~ stat'ctim))
  (define (%call-with-process-io command proc)
    (receive (port process) (open-input-process-port command)
      (proc process port port port)))

  (define (pretty-print obj port)
    (pprint obj :port port))))