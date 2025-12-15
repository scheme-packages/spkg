(define-library (spkg core dependency)
    (import 
      (scheme base)
      (scheme cxr)
      (scheme process-context)
      (scheme write)
      (scheme file)
      (srfi 130)
      (spkg core compat)
      (spkg core log)
      (spkg core lockfile))
  (include "dependency.scm")
  (export 
    cache-dir
    git-dependency-install
    git-dependency-update!
    path-dependency-install
    dependency-needs-recompile?
    git-dependency-install-dir
    git-dependency-manifest-path
    runops
    runops-merge
    filesystem-checksum
    git-dependency?
    git-dependency-name
    git-dependency-url
    git-dependency-target
    git-dependency-subpath
    path-dependency?
    path-dependency-name
    path-dependency-path
    path-dependency-path-set!
    path-dependency-raw?
    path-dependency-manifest-path
    %git-dependency
    %path-dependency
    runops?
    runops-append-path
    runops-prepend-path
    runops-recompile?
    lock-entry->path
    name->path
    name->string
    name->root)
  (begin 

    ;; Git dependency
    ;; Installs into ~/.cache/spkg/dependencies/<name>
    ;; Git repository must contain `spkg.scm` in root directory
    ;; for the package to be recognized. 
    ;;
    ;; subpath is used to specify multiple packages in a single repository.
    (define-record-type <git-dependency>
      (%git-dependency
        name
        url
        target
        subpath)
      git-dependency?
      (name git-dependency-name)
      (url git-dependency-url)
      (target git-dependency-target)
      (subpath git-dependency-subpath))

    ;; Path dependency
    ;; Installs from a local path on the filesystem.
    ;; Path must contain `spkg.scm` in root directory
    ;; for the package to be recognized.
    (define-record-type <path-dependency>
      (%path-dependency
        name
        path
        raw?)
      path-dependency?
      (name path-dependency-name)
      (path path-dependency-path path-dependency-path-set!)
      (raw? path-dependency-raw?))
    
    ;; An installed dependency, regardless of type.
    (define-record-type <runops>
      (runops 
        append-path
        prepend-path
        recompile?)
      runops?
      (append-path runops-append-path)
      (prepend-path runops-prepend-path)
      (recompile? runops-recompile?))

    (define (runops-merge r1 r2)
      (define append-path
        (append (runops-append-path r1)
                (runops-append-path r2)))
      (define prepend-path
        (append (runops-prepend-path r2)
                (runops-prepend-path r1)))
      (define recompile?
        (or (runops-recompile? r1)
            (runops-recompile? r2)))
      (runops append-path prepend-path recompile?))

    ;; TODO: Snowball dependencies: needs parser for snowball
    ;; packages
    ;; TODO: Akku dependencies 
    
  ))