(lambda (record secret)

  ; (define out (open-output-file "testing.txt"))


  (define vars-deploy
       `(lambda (varspath vars) 
          "Write the value to the path. Recursively generate parent
          directories if necessary.
          ; fix the below stuff 
          > record (fnc): library to access record commands
          > codepath (list sym|vec): path to the contract
          > src (exp|sync-pair): contract to be stored at the path
          < return (bool): boolean indicating success of the operation"
          (if (or (null? varspath) (not (eq? (car varspath) '*state*)) (not (eq? (cadr codepath) 'contracts)))
              (error 'path-error "first path segment must be *state* and second must be contracts")
              ((record 'set!) (append '(ledger stage) varspath) vars))))

  ; make this take a code path and a vars path, and the user inputs a hashmap for the var path
  (define contract-deploy
       `(lambda (record codepath src varspath vars) 
          "Write the value to the path. Recursively generate parent
          directories if necessary.
          ; fix the below stuff 
          > record (fnc): library to access record commands
          > codepath (list sym|vec): path to the contract
          > src (exp|sync-pair): contract to be stored at the path
          < return (bool): boolean indicating success of the operation"
          (if (or (null? codepath) (not (eq? (car codepath) '*state*)) (not (eq? (cadr codepath) 'contracts)))
              (error 'path-error "first path segment must be *state* and second must be contracts")
              (begin ((record 'set!) (append '(ledger stage) codepath) src)
                      (,vars-deploy varspath vars)))))


  ; this one works for one method
  ; make this define all the variables (string manipulation), so have str1 = a bunch of defines
  ; then we will have str2 = the code from the code path
  ; then we have the call
  ; so combine str1 and str2 into a begin, and then make the call
  ; get rid of this index parameter 
  (define contract-call
    `(lambda (record codepath varpath index call)
       (let ((ledger ((eval (cadr ((record 'get) '(record library ledger)))) record)))
         (let* ((defs (cadr ((ledger 'get) codepath index)))
                (varsdef (cadr ((ledger 'get) varpath index)))
                (code (cons defs (list call)))
                (varsbf (eval varsdef))
                (defs (eval defs))
                (call-res (eval call))
                (dep (,vars-deploy varpath `(define vars ,vars)))) 
          ;  (display (eval (car code))) 
          ;  (,vars-deploy varpath `(define vars ,vars))
           (format #f "New State: ~a " varsbf)
           
         ))))

  (define contract-call-debug
    `(lambda (record codepath varpath index call)
       (let ((ledger ((eval (cadr ((record 'get) '(record library ledger)))) record)))
         (let* ((defs (cadr ((ledger 'get) codepath index)))
                (varsdef (cadr ((ledger 'get) varpath index)))
                (code (cons defs (list call)))) 
          ;  (display (eval (car code))) 
          ;  (,vars-deploy varpath `(define vars ,vars))
           (format #f "vars before: ~a defs: ~a call result: ~a " varsdef defs call)
           
         ))))
  

  (define mysum `(lambda (num1 num2) (+ num1 num2)))

  (define mysum-call `(lambda (record num1 num2) (,mysum num1 num2)))

  (define test-vote `())

  
  ((record 'set!) '(control local contract-deploy) contract-deploy)
  ((record 'set!) '(control local mysum-call) mysum-call)
  ((record 'set!) '(control local contract-call) contract-call)
  ((record 'set!) '(control local contract-call-debug) contract-call-debug))


