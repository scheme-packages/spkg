(define-library (spkg core log)
  (import (scheme base)
          (scheme process-context)
          (spkg core color)
          (scheme write))
  (export 
    log-level
    log-level:quiet
    log-level:error
    log-level:warning
    log-level:info
    log-level:debug
    info
    errlog
    warn
    verbose)

  (begin 

    (define log-level:quiet 0)
    (define log-level:error 1)
    (define log-level:warning 2)
    (define log-level:info 3)
    (define log-level:debug 4)
    (define log-level (make-parameter log-level:info))

    (define (info header fmt . args)
      (apply logln log-level:info
             (lambda (head) (green (bold head)))
             header 
             fmt args))
    
    (define (errlog header fmt . args)
      (apply logln log-level:error
             (lambda (head) (red (bold head)))
             header 
             fmt args))
    (define (warn header fmt . args)
      (apply logln log-level:warning
             (lambda (head) (yellow (bold head)))
             header 
             fmt args))

    (define (verbose header fmt . args)
      (apply logln log-level:debug
             (lambda (head) (cyan (bold head)))
             header 
             fmt args))

    (define (logln level process-head head fmt . args)
      (apply log level process-head head fmt args)
      (newline (current-error-port)))

    (define (log level process-head head fmt . args)
      (when (>= (log-level) level)
        (let ()
          (define out (current-error-port))
          (define len (string-length fmt))
          (write-colored (process-head head) out)
          (let loop ((i 0) (args args))
            (cond 
              ((< i len)
                (let ((c (string-ref fmt i)))
                  (cond 
                    ((char=? c #\~)
                      (if (>= i len)
                        (error "incomplete format specifier in log"))
                      (let ((next-char (string-ref fmt (+ i 1))))
                        (cond 
                          ((char=? next-char #\~)
                            (write-char #\% out)
                            (loop (+ i 2) args))
                          ((char=? next-char #\a)
                            (display (car args) out)
                            (loop (+ i 2) (cdr args)))
                          ((char=? next-char #\%)
                            (newline out)
                            (loop (+ i 2) args))
                          (else 
                            (error "unknown format specifier" next-char)))))
                    (else 
                      (write-char c out)
                      (loop (+ i 1) args)))))
              (else 
                (flush-output-port out))))))))
)