(define-library (spkg core compat)
  (import (scheme base))
  (cond-expand 
    (capy
      (import (spkg core capy-primitives)))
    (gauche 
      (import (spkg core gauche-primitives)))
    (guile 
      (import (spkg core guile-primitives))))
  (export  
    ; (pretty-print obj) -> void
    pretty-print 
    ; (process command-string) -> (list stdout stdin pid)
    call-with-process-io
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
    canonicalize-path-string))