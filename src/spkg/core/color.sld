(define-library (spkg core color)
  (import (scheme base)
          (scheme process-context)
          (scheme write))
  (export 
    can-color?
    color-mode
    write-colored
    red
    green
    yellow
    blue
    magenta
    cyan
    bold)
  (begin 

    (define-record-type <colored>
      (colored code parent)
      colored?
      (code colored-code)
      (parent colored-parent))

    (define (red str)
      (colored 31 str))
    (define (green str)
      (colored 32 str))
    (define (yellow str)
      (colored 33 str))
    (define (blue str)
      (colored 34 str))
    (define (magenta str)
      (colored 35 str))
    (define (cyan str)
      (colored 36 str))
    (define (bold str)
      (colored 1 str))

    (define (write-colored colored . port?)
      (define port (if (null? port?) (current-output-port) (car port?)))
      (cond 
        ((colored? colored)
          (let ((code (colored-code colored))
                (parent (colored-parent colored)))
            (display (string-append "\x1b;[" (number->string code) "m") port)
            (write-colored parent port)
            (display "\x1b;[0m" port)))
        (else
          (display colored port))))
    

    (define (get-color-mode)
      (define env (get-environment-variable "SPKG_COLOR_MODE"))
      (cond 
        ((not env) 'auto)
        ((or (string-ci=? env "auto") (string-ci=? env "automatic")) 'auto)
        ((or (string-ci=? env "always") (string-ci=? env "true")) 'always)
        ((or (string-ci=? env "never") (string-ci=? env "false")) 'never)
        (else 'auto)))
    
    (define (can-color? port)
      (case (color-mode)
        ((always) #t)
        ((never) #f)
        ((auto) (or 
                  (eq? port (current-output-port))
                  (eq? port (current-error-port))))))
    (define color-mode (make-parameter (get-color-mode)))))