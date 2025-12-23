(define-library (spkg core util)
  (export 
    shell-quote
    system*
    create-directory-recursively
    ensure-directory
    delete-tree
    copy-directory
    capture-required-line
    filesystem-checksum
    verify-checksum
    make-temp-dir
    with-temp-dir)
  (import
    (scheme base)
    (scheme file)
    (scheme write)
    (srfi 130)
    (spkg core compat))
  (begin 

    (define (shell-quote str)
      (let loop ((idx 0) (acc ""))
        (if (= idx (string-length str))
            (string-append "'" acc "'")
            (let* ((ch (string-ref str idx))
                   (fragment (if (char=? ch #\') "'\"'\"'" (string ch))))
              (loop (+ idx 1) (string-append acc fragment))))))

    (define (system* cmd)
      (let ((code (system cmd)))
        (unless (zero? code)
          (error "Command failed" cmd code))))

    (define (create-directory-recursively path)
      (when path
        (system* (string-append "mkdir -p " (shell-quote path)))))

    (define (ensure-directory path)
      (unless (file-exists? path)
        (create-directory-recursively path)))

    (define (delete-tree path)
      (when (and path (file-exists? path))
        (system* (string-append "rm -rf " (shell-quote path)))))

    (define (copy-directory src dst)
      (create-directory-recursively (dirname dst))
      (system* (string-append "cp -R " (shell-quote src) " " (shell-quote dst))))

    (define (capture-required-line cmd)
      (flush-output-port)
      (call-with-process-io
        cmd
        (lambda (pid stdout stdin stderr)
          (define status (process-wait pid))
          (define line (read-line stdout))
          
;          (close-output-port stdin)
          (unless (or (and (boolean? status) status) (zero? status))
            (system* cmd)
            (error "Command failed" cmd status))
          (if (or (eof-object? line) (string=? line ""))
              (error "Command produced no output" cmd)
              line))))

    (define filesystem-checksum-script
      "import hashlib, os, sys\npath = os.path.abspath(sys.argv[1])\nexcludes = set(os.path.normpath(p) for p in sys.argv[2:])\nif not os.path.exists(path):\n    raise SystemExit('missing path: %s' % path)\nh = hashlib.sha256()\n\ndef feed_file(target):\n    with open(target, 'rb') as fh:\n        while True:\n            chunk = fh.read(131072)\n            if not chunk:\n                break\n            h.update(chunk)\nif os.path.isfile(path):\n    relname = os.path.basename(path)\n    if relname not in excludes:\n        feed_file(path)\nelse:\n    for root, dirs, files in os.walk(path):\n        dirs[:] = [d for d in dirs if d != '.git' and os.path.relpath(os.path.join(root, d), path) not in excludes]\n        dirs.sort()\n        files.sort()\n        rel = os.path.relpath(root, path)\n        if rel in excludes:\n            continue\n        h.update(rel.encode())\n        for name in files:\n            fp = os.path.join(root, name)\n            relpath = os.path.relpath(fp, path)\n            if relpath in excludes:\n                continue\n            h.update(relpath.encode())\n            feed_file(fp)\nprint(h.hexdigest())\n")

    (define (filesystem-checksum path . maybe-excludes)
      (define excludes (if (null? maybe-excludes) '() (car maybe-excludes)))
      (define exclude-args
        (if (null? excludes)
            ""
            (string-append " " (string-join (map shell-quote excludes) " "))))
      (capture-required-line
        (string-append "python3 -c \"" filesystem-checksum-script "\" "
                       (shell-quote path)
                       exclude-args)))

    (define (make-temp-dir)
      (capture-required-line "mktemp -d /tmp/spkg.XXXXXX"))

    (define (with-temp-dir proc)
      (let ((dir (make-temp-dir)))
        (dynamic-wind
          (lambda () #f)
          (lambda () (proc dir))
          (lambda () (delete-tree dir)))))

  )
)