(define-library (spkg core errors)
  (import (scheme base)
          (scheme write)
          (scheme file)
          (spkg core log))
  (export
    ;; Base record
    spkg-error?
    make-spkg-error
    spkg-error-kind
    spkg-error-message
    spkg-error-details

    ;; Common constructors (merged categories)
    raise-user-error
    raise-manifest-error
    raise-lockfile-error
    raise-dependency-error
    raise-internal-error

    ;; Printing
    print-condition-to-errlog)

  (begin
    ;; We use a single record type with a "kind" symbol to keep things portable
    ;; and to make it easy to merge multiple historical `(error ...)` sites into
    ;; a small number of categories.
    (define-record-type <spkg-error>
      (make-spkg-error kind message details)
      spkg-error?
      (kind spkg-error-kind)
      (message spkg-error-message)
      (details spkg-error-details))

    (define (raise-user-error message . maybe-details)
      (raise (make-spkg-error 'user message (if (null? maybe-details) #f (car maybe-details)))))

    (define (raise-manifest-error message . maybe-details)
      (raise (make-spkg-error 'manifest message (if (null? maybe-details) #f (car maybe-details)))))

    (define (raise-lockfile-error message . maybe-details)
      (raise (make-spkg-error 'lockfile message (if (null? maybe-details) #f (car maybe-details)))))

    (define (raise-dependency-error message . maybe-details)
      (raise (make-spkg-error 'dependency message (if (null? maybe-details) #f (car maybe-details)))))

    (define (raise-internal-error message . maybe-details)
      (raise (make-spkg-error 'internal message (if (null? maybe-details) #f (car maybe-details)))))

    (define (kind->header k)
      (case k
        ((user) "ERROR ")
        ((manifest) "MANIFEST ")
        ((lockfile) "LOCKFILE ")
        ((dependency) "DEPENDENCY ")
        ((internal) "INTERNAL ")
        (else "ERROR ")))

    (define (obj->string x)
      (let ((port (open-output-string)))
        (write x port)
        (get-output-string port)))


    (define (print-condition-to-errlog c)
      (cond
        ((spkg-error? c)
         (let ((hdr (kind->header (spkg-error-kind c)))
               (msg (spkg-error-message c))
               (det (spkg-error-details c)))
           (if det
               (errlog hdr "~a (~a)" msg det)
               (errlog hdr "~a" msg))))
        (else
         ;; Best-effort fallback for arbitrary raised objects.
         (errlog "ERROR" "Unhandled exception: ~a" c)
         (errlog "ERROR" "Details: ~a" (obj->string c))))))

  )
