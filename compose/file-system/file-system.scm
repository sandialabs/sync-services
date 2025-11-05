;; goals:
;;  - transparently read even non-fs file systems
;;  - support arbitrary metadata
;;  - don't be too linux-y


(lambda (record)

  (define file-system-set-meta!
    '(lambda (record meta)
       ((ledger 'set!) '(*state* *file-system* meta) meta)))

  (define file-system-read-data
    '(lambda (ledger path)
       (let ((result ((ledger 'get) path)))
         (if (and (eq? (car result) 'object) (not (byte-vector? (cadr object))))
             (list (car result) (object->string (cadr result)))
             result))))

  (define file-system-read-meta
    '(lambda (ledger path)
       ((ledger 'get) (append (list (car path) *file-system*) (cdr path) '(*meta*)) meta)))

  (define file-system-read
    `(lambda (ledger path)
       `((data (,,file-system-read-data ledger path))
         (meta (,,file-system-read-meta ledger path)))))

  (define file-system-make-directory!
    '(lambda (ledger path meta)
       ((ledger 'set) path #f)
       ((ledger 'set) (append (list (car path) *file-system*) (cdr path) '(*meta*)) meta)))

  (define file-system-set-meta!
    '(lambda (ledger path value meta)
       ((ledger 'set) (append (list (car path) *fs*) (cdr path)) meta)))

  (define file-system-edit-meta!
    '(lambda (ledger path value meta)
       (let ((meta-path (append (list (car path) *fs*) (cdr path) '(*meta*))))
         (let ((meta-store ((ledger get) meta-path)))
           (make-hash-table meta-old)
           (let loop ((ls meta))
             (if (null? ls) #t
                 (begin (set! (meta-store (caar ls)) (cadar ls))
                        (loop (cdr ls)))))
           (let ((final (map (lambda (x y) (list x y)) meta-store)))
             ((ledger 'set!) meta-path (if (null? final) #f final)))))))

  (define file-system-write-file!
    '(lambda (ledger path value meta)
       (let ((meta-path (append (list (car path) *fs*) (cdr path) '(*meta*))))
         ((ledger 'set!) path value)
         ((ledger 'set!) path meta))))

  (define file-system-append-file!
    '(lambda (ledger path value meta)
       (let ((meta-path (append (list (car path) *fs*) (cdr path) '(*meta*)))
             (previous ((ledger 'get) path)))
         (if (not (byte-vector? previous))
             (error 'type-error "Cannot append to non-byte-vector file"))
         ((ledger 'set!) path (append previous value))
         ((ledger 'set!) path meta))))

  (define file-system-symlink!
    '(lambda (ledger path dest)
       (let ((meta-path (append (list (car path) *fs*) (cdr path) '(*meta*))))
         ((ledger 'set!) meta-path `(symlink ,dest)))))

  (define file-system-rename!
    '(lambda (ledger path dest)
       (let ((meta-path (append (list (car path) *fs*) (cdr path) '(*meta*)))
             (meta-dest (append (list (car path) *fs*) (cdr path) '(*meta*))))
         ((ledger 'set!) path dest)
         ((ledger 'set!) meta-path meta-dest))))

  (define file-system-remove!
    '(lambda (ledger path)
       (let ((meta-path (append (list (car path) *fs*) (cdr path) '(*meta*))))
         ((ledger 'set!) meta-path #f))))

  (let loop ((names '(file-system-set-meta!
                      file-system-read
                      file-system-read-data
                      file-system-read-meta
                      file-system-make-directory!
                      file-system-set-meta!
                      file-system-edit-meta!
                      file-system-write-file!
                      file-system-append-file!)))
    (if (null? names) #t
        (let ((wrapped `(lambda (record) 
                          (let ((ledger ((record 'get) (control library ledger))))
                            (,(eval (car names)) ledger)))))
          ((record 'set!) `(control local ,(car names)) wrapped)
          (loop (cdr names)))))

  "Installed file system extension")
