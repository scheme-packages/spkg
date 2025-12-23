(define-library (spkg core dependency)
    (import 
      (scheme base)
      (scheme cxr)
      (scheme process-context)
      (scheme write)
      (scheme file)
      (srfi 130)
      (spkg core compat)
  (spkg core util)
      (spkg core log)
      (spkg core lockfile))
  (include "dependency.scm")
  (export 
    cache-dir
    git-dependency-install
    git-dependency-update!
    path-dependency-install
    oci-dependency-install
    oci-dependency-update!
    dependency-needs-recompile?
    git-dependency-install-dir
    git-dependency-manifest-path
    oci-dependency-install-dir
    oci-dependency-manifest-path
    runops
    runops-merge
    git-dependency?
    git-dependency-name
    git-dependency-url
    git-dependency-target
    git-dependency-subpath
    oci-dependency?
    oci-dependency-name
    oci-dependency-url
    oci-dependency-target
    oci-dependency-subpath
    path-dependency?
    path-dependency-name
    path-dependency-path
    path-dependency-path-set!
    path-dependency-raw?
    path-dependency-manifest-path
    %git-dependency
    %oci-dependency
    %path-dependency
    runops?
    runops-append-path
    runops-prepend-path
    runops-recompile?
    lock-entry->path
    name->path
    name->string
    name->root
    system-dependencies?
    system-dependencies-names
    system-dependencies
    )
  (begin 

    ;; Git dependency
    ;; Installs into ~/.cache/spkg/dependencies/<name>
    ;; Git repository must contain `spkg.scm` in root directory
    ;; for the package to be recognized. 
    ;;
    ;; subpath selects a package within the extracted Git tree, optional.
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

    ;; OCI dependency
    ;; Installs from an OCI registry via `oras`.
    ;; url is the registry/repo reference (e.g. "ghcr.io/org/pkg").
    ;; target is the tag (e.g. "1.2.3"), optional.
    ;;
    ;; subpath selects a package within the extracted tree, optional.
    (define-record-type <oci-dependency>
      (%oci-dependency
        name
        url
        target
        subpath)
      oci-dependency?
      (name oci-dependency-name)
      (url oci-dependency-url)
      (target oci-dependency-target)
      (subpath oci-dependency-subpath))

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

    ;; A list of system dependencies by name
    ;; These are platform packages that must be installed outside of spkg
    ;; and handled via selected Scheme implementation.
    (define-record-type <system-dependencies>
      (system-dependencies
        names)
      system-dependencies?
      (names system-dependencies-names))

    ;; TODO: Snowball dependencies: needs parser for snowball
    ;; packages
    ;; TODO: Akku dependencies 
    
  ))