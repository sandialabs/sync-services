(lambda (record seret)
  (define file-system-attributes
    '(lambda (ledger path)
       '()))

  (define file-system-dir-read
    '(lambda (ledger path)
       '()))

  (define file-system-dir-make!
    '(lambda (ledger path)
       '()))

  (define file-system-dir-delete!
    '(lambda (ledger path)
       '()))

  (define file-system-file-make!
    '(lambda (ledger path)
       '()))

  (define file-system-file-read
    '(lambda (ledger path)
       '()))

  (define file-system-file-write!
    '(lambda (ledger path value)
       '()))

  (define file-system-file-delete!
    '(lambda (ledger path)
       '()))

  (let loop ((functions '(file-system-attributes
                          file-system-dir-read
                          file-system-dir-make!
                          file-system-dir-delete!
                          file-system-file-make!
                          file-system-file-read
                          file-system-file-write!
                          file-system-file-delet!)))
    (if (null? functions) #t
        ;; todo: wrap the function to access ledger
        ;;  - pass in ledger instead of record
        ;;  - format path into a valid path
        ;;  - format output into "JSON" format
        ((record 'set!) `(control local ,(car functions)) (eval (car functions)))))

  "Installed file system extension")
