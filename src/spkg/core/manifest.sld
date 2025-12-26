(define-library (spkg core manifest)
  (import 
    (scheme base)
    (srfi 1)
    (scheme file)
    (scheme write)
    (spkg core dependency)
    (spkg core log)
    (spkg core compat)
    (scheme read))
  (export 
    package
    manifest

    manifest-path
    manifest-root-directory
    manifest-source-path
    manifest-package
    manifest-dependencies
    manifest-dev-dependencies
    manifest?
    read-manifest

    package?
    package-name
    package-libraries
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
        source-path
        package
        dependencies
        dev-dependencies)
        
      manifest?
      (path manifest-path manifest-path-set!)
      (source-path manifest-source-path)
      (package manifest-package)
      (dependencies manifest-dependencies)
      (dev-dependencies manifest-dev-dependencies))

    (define-record-type <package>
      (%package 
        name 
        libraries
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
      (libraries package-libraries)
      (rnrs package-rnrs)
      (version package-version)
      (authors package-authors)
      (description package-description)
      (documentation package-documentation)
      (license package-license)
      (homepage package-homepage)
      (readme package-readme)
      (repository package-repository))
    
    (define (make-manifest package dependencies dev-dependencies source-path)
      (unless (package? package)
        (error "Invalid package in manifest" package))

      (unless (list? dependencies)
        (error "Dependencies must be a list" dependencies))

      (unless (list? dev-dependencies)
        (error "Dev dependencies must be a list" dev-dependencies))

      (unless (or (not source-path) (string? source-path))
        (error "Manifest source-path must be a string" source-path))

      (%manifest 
        default-name
        (or source-path "src")
        package
        dependencies
        dev-dependencies))

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
                 (m (parse-manifest-exprs exprs)))
            (manifest-path-set! m canonical)
            (manifest-verify m)
            m))))

    (define (manifest-verify m)
      (unless (manifest? m)
        (error "not a valid manifest" m))
      (let* ((path (manifest-path m))
             (deps (manifest-dependencies m))
             (pkg (manifest-package m))
             (dir (manifest-root-directory m))
             (src (manifest-source-path m)))
        (let ()
        (define (verify-dep dep)
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
                  (error "dependency subpath must be a string or #f" (git-dependency-subpath dep))))

              ((oci-dependency? dep)
               (unless (or (list? (oci-dependency-name dep)) (symbol? (oci-dependency-name dep)))
                 (error "dependency name must be a symbol or list" (oci-dependency-name dep)))
               (unless (string? (oci-dependency-url dep))
                 (error "dependency url must be a string" (oci-dependency-url dep)))
               (unless (or (not (oci-dependency-target dep))
                           (string? (oci-dependency-target dep)))
                 (error "dependency target must be a string or #f" (oci-dependency-target dep)))
               (unless (or (not (oci-dependency-subpath dep))
                           (string? (oci-dependency-subpath dep)))
                 (error "dependency subpath must be a string or #f" (oci-dependency-subpath dep))))
              (else #t)))
        (define libs
          (let ((val (package-libraries pkg)))
            (cond
              ((and (list? val) (not (null? val))) val)
              (else (list (package-name pkg))))))
        (define (verify-lib lib)
          (define libfile (string-append dir "/" src "/" (name->path lib)))
          (unless (list? lib)
            (error "library name must be a list of symbols" lib))
          (unless (and (pair? lib) (every symbol? lib))
            (error "library name must be a non-empty list of symbols" lib))
          
          (cond
            ((and (file-exists? (string-append libfile ".sls"))
                  (file-exists? (string-append libfile ".sld")))
             (error "Both .sls and .sld files exist for library, only one allowed:" libfile))
            ((file-exists? (string-append libfile ".sld"))
             (unless (eq? 'r7rs (package-rnrs pkg))
               (error ".sld files only supported for r7rs packages:" libfile)))
            ((file-exists? (string-append libfile ".sls"))
             (unless (eq? 'r6rs (package-rnrs pkg))
               (error ".sls files only supported for r6rs packages:" libfile)))
            (else
             (error "Library file not found for package" lib))))
        (for-each verify-dep deps)
  ;; check that `path/<source-path>/<lib>.sld` or `<lib>.sls` exists for each exported library.
        ;; If no explicit libraries are provided, we default to just the package name.
        (for-each verify-lib libs))))

    (define (manifest-root-directory m)
      (define path (manifest-path m))
      (define dirn (dirname path))
      (if (or (not dirn) (string=? dirn "") (string=? dirn "."))
          "."
          dirn))

    (define (ensure-list expr who)
      (unless (list? expr)
        (error who "must be a list" expr))
      expr)

    (define (ensure-pair expr who)
      (unless (pair? expr)
        (error who "must be a non-empty list" expr))
      expr)

    (define (parse-key-value-clause clause who)
      (ensure-list clause who)
      (ensure-pair clause who)
      (let ((k (car clause))
            (rest (cdr clause)))
        (unless (symbol? k)
          (error who "clause key must be a symbol" clause))
        (values k rest)))

    (define (parse-single clause who)
      (call-with-values
        (lambda () (parse-key-value-clause clause who))
        (lambda (k rest)
          (unless (and (pair? rest) (null? (cdr rest)))
            (error who "clause must have exactly one value" clause))
          (values k (car rest)))))

    (define (normalize-name name who)
      (cond
        ((symbol? name) (list name))
        ((and (list? name) (pair? name) (every symbol? name)) name)
        (else (error who "name must be a symbol or non-empty list of symbols" name))))

    (define (parse-dep-options clauses who)
      (let loop ((xs clauses)
                 (opts '()))
        (if (null? xs)
            (reverse opts)
            (let ((clause (car xs)))
              (call-with-values
                (lambda () (parse-single clause who))
                (lambda (k v)
                  (loop (cdr xs) (cons (cons k v) opts))))))))

    (define (opt-ref opts k)
      (let ((p (assq k opts)))
        (and p (cdr p))))

    (define (parse-git-dep expr)
      (define who "git dependency")
      (ensure-list expr who)
      (ensure-pair expr who)
      (unless (eq? 'git (car expr))
        (error who "expected (git ...)" expr))
      (let* ((opts (parse-dep-options (cdr expr) who))
             (name (opt-ref opts 'name))
             (url (opt-ref opts 'url))
             (rev (opt-ref opts 'rev))
             (subpath (opt-ref opts 'subpath)))
        (%git-dependency
          (and name (normalize-name name who))
          url
          rev
          subpath)))

    (define (parse-oci-dep expr)
      (define who "oci dependency")
      (ensure-list expr who)
      (ensure-pair expr who)
      (unless (eq? 'oci (car expr))
        (error who "expected (oci ...)" expr))
      (let* ((opts (parse-dep-options (cdr expr) who))
             (name (opt-ref opts 'name))
             (url (opt-ref opts 'url))
             (rev (opt-ref opts 'rev))
             (subpath (opt-ref opts 'subpath)))
        (%oci-dependency
          (and name (normalize-name name who))
          url
          rev
          subpath)))

    (define (parse-path-dep expr)
      (define who "path dependency")
      (ensure-list expr who)
      (ensure-pair expr who)
      (unless (eq? 'path (car expr))
        (error who "expected (path ...)" expr))
      (let* ((opts (parse-dep-options (cdr expr) who))
             (name (opt-ref opts 'name))
             (path (opt-ref opts 'path))
             (raw? (opt-ref opts 'raw?)))
        (%path-dependency
          (and name (normalize-name name who))
          (canonicalize-path-string path)
          (if (boolean? raw?) raw? #f))))

    (define (parse-system-dep expr)
      (define who "system dependency")
      (ensure-list expr who)
      (ensure-pair expr who)
      (unless (eq? 'system (car expr))
        (error who "expected (system ...)" expr))
      (let ((names (cdr expr)))
        (unless (every symbol? names)
          (error who "system dependency names must be symbols" names))
        (system-dependencies names)))

    (define (parse-dependency expr)
      (define who "dependency")
      (ensure-list expr who)
      (ensure-pair expr who)
      (case (car expr)
        ((git) (parse-git-dep expr))
        ((oci) (parse-oci-dep expr))
        ((path) (parse-path-dep expr))
        ((system) (parse-system-dep expr))
        (else (error who "unknown dependency type" (car expr) expr))))

    (define (parse-dependencies-block expr who)
      (ensure-list expr who)
      (ensure-pair expr who)
      (unless (eq? who (car expr))
        (error "manifest" "unexpected block" expr))
      (map parse-dependency (cdr expr)))

    (define (parse-package-block expr)
      (define who "package")
      (ensure-list expr who)
      (ensure-pair expr who)
      (unless (eq? 'package (car expr))
        (error who "expected (package ...)" expr))

      (let loop ((clauses (cdr expr))
                 (name #f)
                 (libraries #f)
                 (rnrs 'r7rs)
                 (version #f)
                 (authors '())
                 (description "")
                 (documentation "")
                 (license "")
                 (homepage "")
                 (readme "")
                 (repository ""))
        (if (null? clauses)
            (make-package
              name libraries rnrs version authors description documentation license homepage readme repository)
            (let ((clause (car clauses)))
              (call-with-values
                (lambda () (parse-key-value-clause clause who))
                (lambda (k rest)
                  (case k
                    ((name)
                     (unless (and (pair? rest) (null? (cdr rest)))
                       (error who "(name ...) expects one value" clause))
                     (loop (cdr clauses) (car rest) libraries rnrs version authors description documentation license homepage readme repository))
                    ((libraries)
                     (loop (cdr clauses) name rest rnrs version authors description documentation license homepage readme repository))
                    ((rnrs)
                     (unless (and (pair? rest) (null? (cdr rest)))
                       (error who "(rnrs ...) expects one value" clause))
                     (loop (cdr clauses) name libraries (car rest) version authors description documentation license homepage readme repository))
                    ((version)
                     (unless (and (pair? rest) (null? (cdr rest)))
                       (error who "(version ...) expects one value" clause))
                     (loop (cdr clauses) name libraries rnrs (car rest) authors description documentation license homepage readme repository))
                    ((authors)
                     (unless (and (pair? rest) (null? (cdr rest)))
                       (error who "(authors ...) expects one value" clause))
                     (loop (cdr clauses) name libraries rnrs version (car rest) description documentation license homepage readme repository))
                    ((description)
                     (unless (and (pair? rest) (null? (cdr rest)))
                       (error who "(description ...) expects one value" clause))
                     (loop (cdr clauses) name libraries rnrs version authors (car rest) documentation license homepage readme repository))
                    ((documentation)
                     (unless (and (pair? rest) (null? (cdr rest)))
                       (error who "(documentation ...) expects one value" clause))
                     (loop (cdr clauses) name libraries rnrs version authors description (car rest) license homepage readme repository))
                    ((license)
                     (unless (and (pair? rest) (null? (cdr rest)))
                       (error who "(license ...) expects one value" clause))
                     (loop (cdr clauses) name libraries rnrs version authors description documentation (car rest) homepage readme repository))
                    ((homepage)
                     (unless (and (pair? rest) (null? (cdr rest)))
                       (error who "(homepage ...) expects one value" clause))
                     (loop (cdr clauses) name libraries rnrs version authors description documentation license (car rest) readme repository))
                    ((readme)
                     (unless (and (pair? rest) (null? (cdr rest)))
                       (error who "(readme ...) expects one value" clause))
                     (loop (cdr clauses) name libraries rnrs version authors description documentation license homepage (car rest) repository))
                    ((repository)
                     (unless (and (pair? rest) (null? (cdr rest)))
                       (error who "(repository ...) expects one value" clause))
                     (loop (cdr clauses) name libraries rnrs version authors description documentation license homepage readme (car rest)))
                    (else (error who "unknown package field" k clause)))))))))

    (define (parse-manifest-exprs exprs)
      (define pkg #f)
      (define deps '())
      (define dev-deps '())
      (define source-path #f)
      (for-each
        (lambda (expr)
          (ensure-list expr "manifest")
          (ensure-pair expr "manifest")
          (case (car expr)
            ((package) (set! pkg (parse-package-block expr)))
            ((dependencies) (set! deps (parse-dependencies-block expr 'dependencies)))
            ((dev-dependencies) (set! dev-deps (parse-dependencies-block expr 'dev-dependencies)))
            ((source-path)
             (call-with-values
               (lambda () (parse-single expr "manifest"))
               (lambda (_k v)
                 (unless (string? v)
                   (error "manifest" "(source-path ...) must be a string" expr))
                 (set! source-path v))))
            (else (error "manifest" "unknown top-level form" (car expr) expr))))
        exprs)
      (make-manifest pkg deps dev-deps source-path))

    ;; Exported constructors (procedural; callers must pass data).
    (define (package . clauses)
      (parse-package-block (cons 'package clauses)))

    (define (manifest . exprs)
      (parse-manifest-exprs exprs))


    (define (make-package 
      name libraries rnrs version authors description documentation license homepage readme repository)
      ;; Normalize libraries: (#f or '()) means default to package name.
      (define normalized-libraries
        (cond
          ((or (not libraries) (null? libraries)) '())
          (else
            (map
              (lambda (lib)
                (cond
                  ((symbol? lib) (list lib))
                  ((and (list? lib) (pair? lib) (every symbol? lib)) lib)
                  (else (error "Each library must be a non-empty list of symbols" lib))))
              libraries))))
      (unless version
        (warn "Warning " "No version specified in package manifest. You would not be able to publish this package without a version."))

      (when (symbol? name)
        (set! name (list name)))
      (unless (and (list? name) (pair? name) (every symbol? name))
        (error "Package name must be a non-empty list of symbols" name))

      (unless (or (not libraries)
             (and (list? libraries)
               (every (lambda (x) (or (symbol? x) (list? x))) libraries)))
        (error "Libraries must be a list of library names" libraries))

      
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
        normalized-libraries
        rnrs
        version
        authors
        description
        documentation
        license
        homepage
        readme
        repository))
))