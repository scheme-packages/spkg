(define-library (spkg core compat)
  (import 
    (capy pretty-print)
    (core io process)
    (scheme base)
    (only (scheme file) file-exists?)
    (only (capy) 
      dirname 
      directory-list 
      create-directory 
      delete-file 

      ; (current-directory) -> path ; gets current directory
      ; (current-directory path) ; changes current directory to path
      current-directory 
      file-directory?
      file-symbolic-link?
      change-file-mode
      rename-file
      create-symbolic-link
      create-hard-link
      file-stat-atime ; file-stat-atime(path) -> seconds
      file-stat-mtime ; file-stat-mtime(path) -> seconds
      file-stat-ctime ; file-stat-ctime(path) -> seconds
      getcwd          ; getcwd() -> path
      absolute-path-string?
      canonicalize-path-string

    ))
  
  (export 
    ; (pretty-print obj) -> void
    pretty-print 
    ; (process command-string) -> (list stdout stdin pid)
    process 
    ; (process-wait pid) -> exit-status
    process-wait 
    ; (dirname path) -> path.parent
    dirname 
    ; (directory-list path) -> (list path)
    directory-list 
    ; (system command-string) -> exit-status
    system
    ; (create-directory path)
    create-directory
    ; (delete-file path)
    delete-file
    ; (file-exists? path) -> bool
    file-exists?
    ; (file-directory? path) -> bool
    file-directory?
    ; (file-symbolic-link? path) -> bool    
    file-symbolic-link?
    ; (change-file-mode path mode)
    change-file-mode
    ; (rename-file old-path new-path) 
    rename-file
    ; (create-symbolic-link target-path link-path)
    create-symbolic-link
    ; (create-hard-link target-path link-path)
    create-hard-link
    ; (file-stat-atime path) -> seconds
    file-stat-atime
    ; (file-stat-mtime path) -> seconds
    file-stat-mtime
    ; (file-stat-ctime path) -> seconds
    file-stat-ctime
    ; (current-directory) -> path
    ; (current-directory path)
    current-directory
    ; (getcwd) -> path
    getcwd
    ; (absolute-path-string? path) -> bool
    absolute-path-string?
    canonicalize-path-string)
  
  (begin))