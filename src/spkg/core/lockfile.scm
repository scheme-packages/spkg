(define default-lockfile-name "spkg.lock")
(define lockfile-version 2)

(define-record-type <lock-entry>
  ;; Custom constructor takes either 7 args (raw? defaults to #f)
  ;; or 8 args (explicit raw?).
  (%lock-entry name type url rev checksum subpath source raw?)
  lock-entry?
  (name lock-entry-name)
  (type lock-entry-type)
  (url lock-entry-url)
  (rev lock-entry-rev)
  (checksum lock-entry-checksum)
  (subpath lock-entry-subpath)
  (source lock-entry-source)
  (raw? lock-entry-raw?))

(define (make-lock-entry name type url rev checksum subpath source . maybe-raw?)
  (define raw? (if (null? maybe-raw?) #f (car maybe-raw?)))
  (%lock-entry name type url rev checksum subpath source raw?))

(define-record-type <lockfile>
  (%lockfile path root-checksum entries)
  lockfile?
  (path lockfile-path)
  (root-checksum lockfile-root-checksum lockfile-root-checksum-set!)
  (entries lockfile-entries lockfile-entries-set!))

(define (make-lockfile path . maybe-root)
  (define root (if (null? maybe-root) #f (car maybe-root)))
  (%lockfile path root '()))

(define (load-lockfile path)
  (cond 
    ((and path (file-exists? path))
      (call-with-input-file path
        (lambda (in)
          (let ((expr (read in)))
            (if (eof-object? expr)
                (make-lockfile path)
                (parse-lockfile path expr))))))
    (else (make-lockfile path))))

(define (save-lockfile! lock)
  ;; Write atomically to avoid corrupting the lockfile on interruption.
  ;; Also enforce deterministic entry ordering for stable diffs.
  (define path (lockfile-path lock))
  (define tmp (string-append path ".tmp"))
  (call-with-output-file tmp
    (lambda (out)
      (pretty-print (lockfile->sexp lock) out)
      (newline out)))
  (rename-file tmp path))

;; Canonical entry-name ordering to keep lockfile output stable.
(define (lock-name->string name)
  (cond
    ((symbol? name) (symbol->string name))
    ((list? name) (string-join (map symbol->string name) "/"))
    (else "")))

(define (lockfile-sorted-entries lock)
  (list-sort
    (lambda (a b)
      (string<? (lock-name->string (car a))
                (lock-name->string (car b))))
    (lockfile-entries lock)))

(define (lockfile->sexp lock)
  (let ((entries (map cdr (lockfile-sorted-entries lock))))
    (cons 'lockfile
      (append `((version ,lockfile-version)
        ,@(if (lockfile-root-checksum lock)
          `((root-checksum ,(lockfile-root-checksum lock)))
          '()))
      (map lock-entry->sexp entries)))))

(define (lockfile-ref lock name)
  ;; Raw deps are normalized to list-form keys in the lockfile.
  ;; For lookups, try exact key first, then fall back to list normalization
  ;; for the symbol<->(symbol) single-segment case.
  (let* ((entries (lockfile-entries lock))
         (pair (assoc name entries)))
    (cond
      (pair (cdr pair))
      ((symbol? name)
       (let ((pair2 (assoc (list name) entries)))
         (and pair2 (cdr pair2))))
      ((and (list? name) (pair? name) (null? (cdr name)))
       (let ((pair2 (assoc (car name) entries)))
         (and pair2 (cdr pair2))))
      (else #f))))

(define (lockfile-set-entry! lock entry)
  ;; Canonicalize names so we never store two entries for the same dependency
  ;; due to the historical symbol vs (symbol) raw-dep normalization.
  ;; New canonical form: ALWAYS store names as lists so multi-segment
  ;; package names like `(foreign c)` are first-class.
  (define (canonical-name name)
    (cond
      ((symbol? name) (list name))
      ;; Keep list names (including multi-segment) as-is.
      (else name)))

  (define name (canonical-name (lock-entry-name entry)))

  ;; Ensure stored entry carries canonical name too (important for printing).
  (when (not (equal? (lock-entry-name entry) name))
    (set! entry (make-lock-entry
                  name
                  (lock-entry-type entry)
                  (lock-entry-url entry)
                  (lock-entry-rev entry)
                  (lock-entry-checksum entry)
                  (lock-entry-subpath entry)
                  (lock-entry-source entry)
                  (lock-entry-raw? entry))))

  (define entries (lockfile-entries lock))
  (define existing (assoc name entries))

  (let ((alt (and (list? name) (pair? name) (null? (cdr name)) (car name))))
    (when alt
      (let ((legacy (assoc alt entries)))
        (when legacy
          (lockfile-entries-set! lock
            (filter (lambda (p) (not (equal? (car p) alt)))
                    (lockfile-entries lock)))
          (set! entries (lockfile-entries lock))))))

  ;; Prefer non-raw metadata if we’re overwriting an existing entry.
  (when existing
    (let ((old (cdr existing)))
      (when (and old (lock-entry-raw? old) (not (lock-entry-raw? entry)))
        ;; keep incoming (non-raw) entry
        #t)
      (when (and old (not (lock-entry-raw? old)) (lock-entry-raw? entry))
        ;; keep existing (non-raw) entry
        (set! entry old))))

  (if existing
      (set-cdr! existing entry)
      (lockfile-entries-set! lock (append entries (list (cons name entry)))))
  entry)

(define (lock-entry->sexp entry)
  (list (lock-entry-type entry)
        (list 'name (lock-entry-name entry))
        (list 'raw? (lock-entry-raw? entry))
        (list 'url (lock-entry-url entry))
        (list 'rev (lock-entry-rev entry))
        (list 'checksum (lock-entry-checksum entry))
        (list 'subpath (lock-entry-subpath entry))
        (list 'source (lock-entry-source entry))))

(define (parse-lockfile path expr)
  (unless (and (pair? expr) (symbol? (car expr)) (eq? (car expr) 'lockfile))
    (error "Invalid lockfile root" expr))
  (let ((lock (make-lockfile path)))
    (define version-seen #f)
    (let loop ((rest (cdr expr)))
      (cond
        ((null? rest)
         (when (and version-seen (not (or (= version-seen 1) (= version-seen lockfile-version))))
           (error "Unsupported lockfile version" version-seen))
         lock)
        ((and (pair? (car rest)) (symbol? (caar rest)) (eq? (caar rest) 'version))
         (let ((v (cadar rest)))
           (unless (integer? v)
             (error "Invalid lockfile version" v))
           (set! version-seen v)
           (loop (cdr rest))))
        ((and (pair? (car rest)) (symbol? (caar rest)) (eq? (caar rest) 'root-checksum))
         (let ((root (cadar rest)))
           (unless (or (not root) (string? root))
             (error "Invalid lockfile root checksum" root))
           (lockfile-root-checksum-set! lock root)
           (loop (cdr rest))))
        (else
          (lockfile-set-entry! lock (parse-lock-entry (car rest)))
          (loop (cdr rest)))))))

(define (parse-lock-entry expr)
  (unless (and (pair? expr) (symbol? (car expr)))
    (error "Invalid lockfile entry" expr))
  (let* ((entry-type (car expr))
         (fields (cdr expr))
         (alist (map lockfile-field->pair fields))
         (name (lockfile-field-ref 'name alist #f))
         ;; Backwards-compatible: if missing, default to #f.
         (raw? (lockfile-field-ref 'raw? alist #f))
         (url (lockfile-field-ref 'url alist #f))
         (rev (lockfile-field-ref 'rev alist #f))
         (checksum (lockfile-field-ref 'checksum alist #f))
         (subpath (lockfile-field-ref 'subpath alist #f))
         (source (lockfile-field-ref 'source alist #f)))
    (unless (or (symbol? name) (list? name))
      (error "Lockfile entry missing name" expr))
    (unless (string? url)
      (error "Lockfile entry missing url" expr))
    (unless (string? checksum)
      (error "Lockfile entry missing checksum" expr))
    (unless (or (boolean? raw?) (not raw?))
      (error "Invalid lockfile raw? value" raw?))
    (unless (or (not source) (string? source))
      (error "Invalid lockfile source" expr))
    (unless (or (not rev) (string? rev))
      (error "Invalid lockfile rev value" rev))
    (unless (or (not subpath) (string? subpath))
      (error "Invalid lockfile subpath value" subpath))
  (make-lock-entry name entry-type url rev checksum subpath source raw?)))

(define (lockfile-field->pair field)
  (unless (and (pair? field) (pair? (cdr field)) (null? (cddr field)))
    (error "Invalid lockfile field" field))
  (cons (car field) (cadr field)))

(define (lockfile-field-ref key alist default)
  (let ((pair (assoc key alist)))
    (if pair (cdr pair) default)))
