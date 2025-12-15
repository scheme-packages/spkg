(define-library (spkg core manifest)
  (import 
    (scheme base)
    (scheme file)
    (scheme write)
    (spkg core dependency)
    (spkg core log)
    (spkg core compat)
    (scheme read)
    (scheme eval))
  (export 
    package
    manifest

    manifest-path
    manifest-root-directory
    manifest-package
    manifest-dependencies
    manifest?
    read-manifest

    package?
    package-name
    package-rnrs
    package-version
    package-authors
    package-description
    package-documentation
    package-license
    package-homepage
    package-readme
    package-repository)

  (begin 

    ;; Default manifest file name
    (define default-name "spkg.scm")

    ;; Package manifest
    (define-record-type <manifest>
      (%manifest 
        path 
        package
        dependencies)
        
      manifest?
      (path manifest-path manifest-path-set!)
      (package manifest-package)
      (dependencies manifest-dependencies))

    (define-record-type <package>
      (%package 
        name 
        rnrs ; RnRS version: r5rs, r6rs, r7rs
        version ; package version
        authors
        description
        documentation
        license
        homepage
        readme
        repository)
      package?
      (name package-name)
      (rnrs package-rnrs)
      (version package-version))
    
    (define (make-manifest package dependencies)
      (unless (package? package)
        (error "Invalid package in manifest" package))

      (unless (list? dependencies)
        (error "Dependencies must be a list" dependencies))
      
      (%manifest 
        default-name
        package
        dependencies))

    (define (read-manifest path)
      (define canonical (canonicalize-path-string path))
      (call-with-input-file canonical 
        (lambda (in)
          (define (read-all)
            (let loop ((expr (read in)) 
                       (exprs '()))
              (cond 
                ((eof-object? expr) (reverse exprs))
                (else (loop (read in) (cons expr exprs))))))
          
          (let* ((exprs (read-all))
              (m (eval `(manifest ,@exprs))))
                 
            (manifest-path-set! m canonical)
            (manifest-verify m)
            m))))

    (define (manifest-verify m)
      (unless (manifest? m)
        (error "not a valid manifest" m))
      (let* ((path (manifest-path m))
             (deps (manifest-dependencies m))
             (pkg (manifest-package m))
             (dir (manifest-root-directory m)))
        (for-each 
          (lambda (dep)
            (cond 
              ((git-dependency? dep)
                (unless (or (list? (git-dependency-name dep)) (symbol? (git-dependency-name dep)))
                  (error "dependency name must be a symbol" (git-dependency-name dep)))
                (unless (string? (git-dependency-url dep))
                  (error "dependency url must be a string" (git-dependency-url dep)))
                (unless (or (not (git-dependency-target dep))
                            (string? (git-dependency-target dep)))
                  (error "dependency target must be a string or #f" (git-dependency-target dep)))
                (unless (or (not (git-dependency-subpath dep))
                            (string? (git-dependency-subpath dep)))
                  (error "dependency subpath must be a string or #f" (git-dependency-subpath dep)))
                    )))
          deps)
        ;; check that `path/src/pkg.sld  or `pkg.sls` exists
        (define libfile (string-append dir "/src/" (name->path (package-name pkg))))
     
        (cond
          ((and (file-exists? (string-append libfile ".sls"))
                (file-exists? (string-append libfile ".sld")))
           (error "Both .sls and .sld files exist for package, only one allowed:" libfile))
          ((file-exists? (string-append libfile ".sld"))
            (unless (eq? 'r7rs (package-rnrs pkg))
              (error ".sld files only supported for r7rs packages:" libfile)))
          ((file-exists? (string-append libfile ".sls"))
            (unless (eq? 'r6rs (package-rnrs pkg))
              (error ".sls files only supported for r6rs packages:" libfile)))
          (else 
            (error "Library file not found for package" (package-name pkg))))))

    (define (manifest-root-directory m)
      (define path (manifest-path m))
      (define dirn (dirname path))
      (if (or (not dirn) (string=? dirn "") (string=? dirn "."))
          "."
          dirn))

    ;; macros to parse spkg manifest files
    ;; 
    ;; Manifests are just regular Scheme files
    ;; that we read, and `eval` to get the package. 
    ;; User cannot construct package and manifest
    ;; in "raw" way, but only via these macros.

    ;; "main" macro which wraps the source file
    (define-syntax manifest 
      (syntax-rules ()
        ((_ expr ...)
          (manifest-aux (#f '()) expr ...))))

    (define-syntax manifest-aux
      (syntax-rules (package dependencies)
        ((_ (%package %dependencies))
          (make-manifest 
            %package
            %dependencies))
        ((_ (%package %dependencies) (dependencies dep ...) rest ...)
          (manifest-aux 
            (%package 
             (dependencies-aux () dep ...))
             rest ...
            ))
        ((_ (%package %dependencies) (package expr ...) rest ...)
          (manifest-aux 
            ((package expr ...)
             %dependencies)
            rest ...))
        ))


    (define (make-package 
      name rnrs version authors description documentation license homepage readme repository)
      (unless (or (list? name) (symbol? name))
        (error "Package name must be a symbol or list of symbols" name))
      (unless (memq rnrs '(r5rs r6rs r7rs))
        (error "RnRS version must be one of 'r5rs, 'r6rs, 'r7rs" rnrs))

      (unless (list? authors)
        (error "Authors must be a list of strings" authors))

      (unless (or (string? description) (equal? description ""))
        (error "Description must be a string" description))

      (unless (or (string? documentation) (equal? documentation "") )
        (error "Documentation must be a string" documentation))

      (unless (or (string? license) (equal? license ""))
        (error "License must be a string" license))

      (unless (or (string? homepage) (equal? homepage ""))
        (error "Homepage must be a string" homepage))
      (unless (or (string? readme) (equal? readme ""))
        (error "Readme must be a string" readme))
      (unless (or (string? repository) (equal? repository ""))
        (error "Repository must be a string" repository)) 
      

      (%package 
        name 
        rnrs
        version
        authors
        description
        documentation
        license
        homepage
        readme
        repository))
    

    (define-syntax package 
      (syntax-rules () 
        ((_ expr ...)
          (package-aux (
            #f  ;; name 
            'r7rs  ;; RnRs
            #f  ;; version
            '() ;; authors 
            ""  ;; description
            ""  ;; documentation link
            ""  ;; license
            ""  ;; homepage
            ""  ;; readme
            ""  ;; repository
            ) expr ...))))

    (define-syntax package-aux 
      (syntax-rules 
        (name rnrs version authors description documentation license homepage readme repository)
        
        ((_ (%name %rnrs %version %authors %description %documentation %license %homepage %readme %repository))
        
          (make-package 
            %name 
            %rnrs
            %version
            %authors
            %description
            %documentation
            %license
            %homepage
            %readme
            %repository))
        ((_ (%name %rnrs %version %authors %description %documentation %license %homepage %readme %repository)
          (name val) rest ...)
          
          (package-aux 
            ('val 
              %rnrs
              %version
              %authors
              %description
              %documentation
              %license
              %homepage
              %readme
              %repository)
              rest ...))
        ((_ (%name %rnrs %version %authors %description %documentation %license %homepage %readme %repository)
          (rnrs val) rest ...)
          
          (package-aux 
            (%name 
              'val
              %version
              %authors
              %description
              %documentation
              %license
              %homepage
              %readme
              %repository)
              rest ...))

        ((_ (%name %rnrs %version %authors %description %documentation %license %homepage %readme %repository)
          (version val) rest ...)
          
          (package-aux 
            (%name 
              %rnrs
              val
              %authors
              %description
              %documentation
              %license
              %homepage
              %readme
              %repository)
              rest ...))

        ((_ (%name %rnrs %version %authors %description %documentation %license %homepage %readme %repository)
          (authors val) rest ...)
          
          (package-aux 
            (%name 
              %rnrs
              %version
              val
              %description
              %documentation
              %license
              %homepage
              %readme
              %repository)
              rest ...))

        ((_ (%name %rnrs %version %authors %description %documentation %license %homepage %readme %repository)
          (description val) rest ...)
          
          (package-aux 
            (%name 
              %rnrs
              %version
              %authors
              val
              %documentation
              %license
              %homepage
              %readme
              %repository)
              rest ...))

        ((_ (%name %rnrs %version %authors %description %documentation %license %homepage %readme %repository)
          (documentation val) rest ...)
          
          (package-aux 
            (%name 
              %rnrs
              %version
              %authors
              %description
              val
              %license
              %homepage
              %readme
              %repository)
              rest ...))

        ((_ (%name %rnrs %version %authors %description %documentation %license %homepage %readme %repository)
          (license val) rest ...)
          
          (package-aux 
            (%name 
              %rnrs
              %version
              %authors
              %description
              %documentation
              val
              %homepage
              %readme
              %repository)
              rest ...))

        ((_ (%name %rnrs %version %authors %description %documentation %license %homepage %readme %repository)
          (homepage val) rest ...)
          
          (package-aux 
            (%name 
              %rnrs
              %version
              %authors
              %description
              %documentation
              %license
              val
              %readme
              %repository)
              rest ...))

        ((_ (%name %rnrs %version %authors %description %documentation %license %homepage %readme %repository)
          (readme val) rest ...)
          
          (package-aux 
            (%name 
              %rnrs
              %version
              %authors
              %description
              %documentation
              %license
              %homepage
              val
              %repository)
              rest ...))

        ((_ (%name %rnrs %version %authors %description %documentation %license %homepage %readme %repository)
          (repository val) rest ...)
          
          (package-aux 
            (%name 
              %rnrs
              %version
              %authors
              %description
              %documentation
              %license
              %homepage
              %readme
              val)
              rest ...))

        ((_ (%name %rnrs %version %authors %description %documentation %license %homepage %readme %repository))
          (%package %name %rnrs %version))))
          
  
    
    (define-syntax dependencies-aux
      (syntax-rules (git path) 
        ((_ (parsed ...) )
          (list parsed ...))
        ((_ (parsed ...) (git expr ...) rest ...)
          (dependencies-aux 
            (parsed ... (git-dependency expr ...) )
            rest ...))
        ((_ (parsed ...) (path expr ...) rest ...)
          (dependencies-aux 
            (parsed ... (path-dependency expr ...) )
            rest ...))))

    (define-syntax path-dependency 
      (syntax-rules () 
        ((_ expr ...)
          (path-dependency-aux 
            (
              #f  ;; name 
              #f  ;; path 
              #f  ;; raw?
            ) expr ...))))
    
    (define-syntax path-dependency-aux
      (syntax-rules (name path raw?)
        ((_ (%name %path %raw?))
          (%path-dependency 
            %name 
            (canonicalize-path-string %path) 
            %raw?))
        ((_ (%name %path %raw?) (name val) rest ...)
          (path-dependency-aux 
            ('val 
             %path 
             %raw?)
            rest ...))
        ((_ (%name %path %raw?) (path val) rest ...)
          (path-dependency-aux 
            (%name 
             val  
              %raw?)))))

    (define-syntax git-dependency 
      (syntax-rules () 
        ((_ expr ...)
          (git-dependency-aux 
            (
              #f  ;; name 
              #f  ;; url 
              #f  ;; rev
              #f  ;; subpath
            ) expr ...))))

    (define-syntax git-dependency-aux
      (syntax-rules (name url rev subpath)
        ((_ (%name %url %target %subpath))
          (%git-dependency 
            %name 
            %url 
            %target
            %subpath))
        ((_ (%name %url %target %subpath) (name val) rest ...)
          (git-dependency-aux 
            ('val 
             %url 
             %target
             %subpath)
            rest ...))
        ((_ (%name %url %target %subpath) (url val) rest ...)
          (git-dependency-aux 
            (%name 
             val 
             %target
             %subpath)
            rest ...))
        ((_ (%name %url %target %subpath) (rev val) rest ...)
          (git-dependency-aux 
            (%name 
             %url 
             val
             %subpath)
            rest ...))
        ((_ (%name %url %target %subpath) (subpath val) rest ...)
          (git-dependency-aux 
            (%name 
             %url 
             %target
             val)
            rest ...)))
)))