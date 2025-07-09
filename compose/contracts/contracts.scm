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

  (define contract-auth
    `(lambda (record username password index)
      (let ((ledger ((eval (cadr ((record 'get) '(record library ledger)))) record)))
             (let ((table (car ((ledger 'get) `(*state* data accounts) index))))
                  (if (eq? table 'nothing)
                      ; (display table)
                      (display "account does not exist")
                      (let ((table ((ledger 'get) `(*state* data accounts) index))) 
                            (begin (eval (cadr table))
                              ; (format #f "table: ~a current pass: ~a" accounts (accounts username))
                              (if (eq? (accounts username) password)
                                #t
                                (format #f "~a does not equal ~a" (accounts username) password))
                
                                ))))))) 


  ; this one works for one method
  ; make this define all the variables (string manipulation), so have str1 = a bunch of defines
  ; then we will have str2 = the code from the code path
  ; then we have the call
  ; so combine str1 and str2 into a begin, and then make the call
  ; get rid of this index parameter 
  (define contract-call
    `(lambda (record codepath varpath index call)
       (let ((contract-auth
          (lambda (username password)
          (let ((ledger ((eval (cadr ((record 'get) '(record library ledger)))) record)))
                  (let ((table (car ((ledger 'get) `(*state* data accounts) index))))
                      (if (eq? table 'nothing)
                          ; (display table)
                          (display "account does not exist")
                          (let ((table ((ledger 'get) `(*state* data accounts) index))) 
                                  (begin (eval (cadr table))
                                  ; (format #f "table: ~a current pass: ~a" accounts (accounts username))
                                  (if (eq? (accounts username) #f) (error "account does not exist")
                                  (if (string=? (accounts username) password)
                                      #t
                                      (error #f "~a does not equal ~a" (accounts username) password)))
                                      ))))))))
       (let ((ledger ((eval (cadr ((record 'get) '(record library ledger)))) record)))
         (let* ((defs (cadr ((ledger 'get) codepath index)))
                (varsdef (cadr ((ledger 'get) varpath index)))
                (code (cons defs (list call)))
                (varsbf (eval varsdef))
                (defs (eval defs)) ; eval defs 
                (call-res (eval call)) ; eval call
                ; (defstest (eval '(begin (define vote (lambda (user pass) (if (eq? (,contract-auth user pass #f) #t) (set! (vars 'votes) (+ 1 (vars 'votes))) (display "authentication error"))))) ))
                ; (call-res (eval '(vote "divya" "password"))) ; test
                (dep (,vars-deploy varpath `(define vars ,vars)))) 
          ;  (display (eval (car code))) 
          ;  (,vars-deploy varpath `(define vars ,vars)) 
           (format #f "defs: ~a call: ~a" defs call-res)
           
         )))))

  

  (define create-account 
    `(lambda (record username password index)
        (let ((ledger ((eval (cadr ((record 'get) '(record library ledger)))) record)))
             (let ((table (car ((ledger 'get) `(*state* data accounts) index))))
                  (if (eq? table 'nothing)
                      ; (display table)
                      ((record 'set!) (append '(ledger stage) `(*state* data accounts)) `(define accounts (hash-table ,username ,password)))
                      (let ((table ((ledger 'get) `(*state* data accounts) index))) 
                            (begin (eval (cadr table))
                              ; (format #f "table: ~a current pass: ~a" accounts (accounts username))
                              (if (eq? (accounts username) #f)
                                (begin (set! (accounts username) password) ((record 'set!) (append '(ledger stage) `(*state* data accounts)) `(define accounts ,accounts)))
                                (error "username already exists"))
                              (display accounts)
                                )))))))

  

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
  ((record 'set!) '(control local contract-call-debug) contract-call-debug)
  ((record 'set!) '(control local create-account) create-account)
  ; ((record 'set!) '(control local contract-auth) contract-auth)
  )


