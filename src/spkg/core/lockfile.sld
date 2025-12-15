
;;  Lockfile implementation
;;  Lockfiles are implemented as a file which stores
;;  each dependency along with its checksum and source. 


(define-library (spkg core lockfile)
  (import (scheme base)
          (scheme file)
          (scheme write)
          (scheme read)
          (scheme cxr)
          (spkg core compat)
          (srfi 132)
          (srfi 130))
  (export
    lock-name->string
    lockfile-sorted-entries
    default-lockfile-name
    make-lockfile
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
    lock-entry-source
    lock-entry-raw?)
  (include "lockfile.scm"))