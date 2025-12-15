
;;  Lockfile implementation
;;  Lockfiles are implemented as a file which stores
;;  each dependency along with its checksum and source. 


(define-library (spkg core lockfile)
  (import (scheme base)
          (scheme file)
          (scheme write)
          (scheme read)
          (scheme cxr)
          (spkg core compat))
  (export
    default-lockfile-name
    load-lockfile
    save-lockfile!
    lockfile?
    lockfile-path
    lockfile-root-checksum
    lockfile-root-checksum-set!
    lockfile-entries
    lockfile-ref
    lockfile-set-entry!
    lock-entry?
    make-lock-entry
    lock-entry-name
    lock-entry-type
    lock-entry-url
    lock-entry-rev
    lock-entry-checksum
    lock-entry-subpath
    lock-entry-source)
  (include "lockfile.scm"))