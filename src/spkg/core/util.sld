(define-library (spkg core util)
  (export 
    verify-checksum)
  (import (spkg core compat))
  (begin 
    
    
    (define (verify-checksum chksum-path)
      (cond 
        ((file-exists? chksum-path)
          (zero? (system (string-append "sha256sum -c " chksum-path))))
        (else #f)))
    
    (define (checksum-for-directory dir-path output-path)
      (system (string-append "find " dir-path " -type f -exec sha256sum {} + > " output-path)))
  )
)