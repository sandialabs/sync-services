(lambda (start-amt refresh-pd)

  `(lambda (record secret)

  ; (define out (open-output-file "testing.txt"))

    (define contract-auth 
      `(lambda (username password)
            (let ((ledger ((eval (cadr ((record 'get) '(record library ledger)))) record)))
                    (let ((table (car ((ledger 'get) `(*state* data accounts) #f))))
                        (if (eq? table 'nothing)
                            ; (display table)
                            (error 'auth-error "Account does not exist")
                            (let ((table ((ledger 'get) `(*state* data accounts) #f))) 
                                    (begin (eval (cadr table))
                                    ; (format #f "table: ~a current pass: ~a" accounts (accounts username))
                                    (if (eq? (accounts username) #f) (error 'auth-error "account does not exist")
                                    (if (string=? (accounts username) password)
                                        #t
                                        (error 'auth-error "Wrong password")))
                                        )))))))


    (define vars-deploy
        `(lambda (varspath vars) 
            "Write the value to the path. Recursively generate parent
            directories if necessary.
            ; fix the below stuff 
            > record (fnc): library to access record commands
            > codepath (list sym|vec): path to the contract
            > src (exp|sync-pair): contract to be stored at the path
            < return (bool): boolean indicating success of the operation"
            (if (or (null? varspath) (not (eq? (car varspath) '*state*)) (not (eq? (cadr varspath) 'contracts)))
                (error 'path-error "first path segment must be *state* and second must be contracts")
                ((record 'set!) (append '(ledger stage) varspath) vars))))

    ; make this take a code path and a vars path, and the user inputs a hashmap for the var path
    (define contract-deploy
        `(lambda (record username password path src)  
            "Write the value to the path. Recursively generate parent
            directories if necessary.
            ; fix the below stuff 
            > record (fnc): library to access record commands
            > codepath (list sym|vec): path to the contract
            > src (exp|sync-pair): contract to be stored at the path
            < return (bool): boolean indicating success of the operation"
            (let* ((ledger ((eval (cadr ((record 'get) '(record library ledger)))) record)) ; get stuff off the journal 
                (whee (,contract-auth username password))
                (tokens-table ((ledger 'get) `(*state* data tokens) #f))
                (tokens-table (eval (cadr tokens-table))) 
                (times-table ((ledger 'get) `(*state* data times) #f))
                (times-table (eval (cadr times-table))))
            (begin (set! (tokens username) (if (> (- (*s7* 'cpu-time) (times username)) ,,refresh-pd) ; if enough time has elapsed
                                              (begin (set! (times username) (*s7* 'cpu-time)) ; update the time of token giving
                                                    ((record 'set!) (append '(ledger stage) `(*state* data times)) `(define times ,times)) ; redeploy times and tokens tables 
                                                    ((record 'set!) (append '(ledger stage) `(*state* data tokens)) `(define tokens ,tokens)) 
                                                    ,,start-amt) ; refresh time, tokens=10
                                              (tokens username)))
            (define starttime (*s7* 'cpu-time))

            (if (or (null? path) (not (eq? (car path) '*state*)) (not (eq? (cadr path) 'contracts)))
                (error 'path-error "first path segment must be *state* and second must be contracts")
                (begin ((record 'set!) (append '(ledger stage) (append path '(code))) (append '(begin) (cddr src))) ; change these to like append or wtv
                        (,vars-deploy (append path '(vars)) (cadr src))))
            (define time (- (*s7* 'cpu-time) starttime))
            (if (< (tokens username) (* 10000 time)) 
                (begin ((record 'set!) (append '(ledger stage) (append path '(vars))) '#f)
                      (,vars-deploy (append path '(vars)) '#f)
                      (format #f "Insufficient tokens, ~a tokens are required" (* 10000 time))) 
                (begin (set! (tokens username) (- (tokens username) (* 10000 time)))
                    ((record 'set!) (append '(ledger stage) `(*state* data tokens)) `(define tokens ,tokens))
                    (format #f "Success! Remaining tokens: ~a" (tokens username))))
                        ))))


    ; get rid of this index parameter 
    (define contract-call
      `(lambda (record username password path index call)
        (begin (define deploy-later (hash-table)) 
          (let (
              (cross-call (lambda (c-path c-index c-call)
                (let* ((ledger ((eval (cadr ((record 'get) '(record library ledger)))) record))
                        (defs (cadr ((ledger 'get) (append c-path '(code)) c-index)))
                        (varsdef (cadr ((ledger 'get) (append c-path '(vars)) c-index)))
                        (code (cons defs (list c-call)))
                        (varsbf (eval varsdef))
                        (defs2 (eval defs)) ; eval defs 
                        (call-res (eval c-call)) ; eval call
                        (varsaf vars)
                        (wiwi (+ 5 5))
                        ; (dep (,vars-deploy c-varpath `(define vars ,vars)))
                        ) (begin (set! (deploy-later (append c-path '(vars))) `(define vars ,vars)) (format #f "Call result: ~a" call-res))))))
        (let* ((ledger ((eval (cadr ((record 'get) '(record library ledger)))) record)) ; get stuff off the journal 
                (whee (,contract-auth username password))
                (tokens-table ((ledger 'get) `(*state* data tokens) index))
                (tokens-table (eval (cadr tokens-table)))
                (times-table ((ledger 'get) `(*state* data times) index))
                (times-table (eval (cadr times-table))))
            (begin (set! (tokens username) (if (> (- (*s7* 'cpu-time) (times username)) ,,refresh-pd) ; if enough time has elapsed
                                              (begin (set! (times username) (*s7* 'cpu-time)) ; update the time of token giving
                                                    ((record 'set!) (append '(ledger stage) `(*state* data times)) `(define times ,times)) ; redeploy times and tokens tables 
                                                    ((record 'set!) (append '(ledger stage) `(*state* data tokens)) `(define tokens ,tokens)) 
                                                    ,,start-amt) ; refresh time, tokens=10
                                              (tokens username))) ; otherwise keep tokens the same 
          (let* ((defs (cadr ((ledger 'get) (append path '(code)) index)))
                  (start (*s7* 'cpu-time))
                  (varsdef (cadr ((ledger 'get) (append path '(vars)) index)))
                  (code (cons defs (list call)))
                  (varsbf (eval varsdef))
                  (defs (eval defs)) ; eval defs 
                  (call-res (eval call)) ; eval call
                  (time (- (*s7* 'cpu-time) start))
                  ) 
            (if (< (tokens username) (* 10000 time)) (format #f "Insufficient tokens, ~a tokens are required" (* 10000 time)) 
            (begin (,vars-deploy (append path '(vars)) `(define vars ,vars)) 
                    (for-each (lambda (entry) (,vars-deploy (car entry) (cdr entry))) deploy-later)
                    (set! (tokens username) (- (tokens username) (* 10000 time)))
                    ((record 'set!) (append '(ledger stage) `(*state* data tokens)) `(define tokens ,tokens))
                    (format #f "call: ~a time: ~a tokens ~a deploy-later: ~a" call-res time (tokens username) deploy-later)
                    ))
          ))))) ))

    

    (define create-account 
      '(lambda (record username password index)
          (let ((ledger ((eval (cadr ((record 'get) '(record library ledger)))) record)))
              (let ((table (car ((ledger 'get) `(*state* data accounts) index))))
                    (if (eq? table 'nothing)
                        ; (display table)
                        (begin ((record 'set!) (append '(ledger stage) `(*state* data accounts)) `(define accounts (hash-table ,username ,password)))
                              ((record 'set!) (append '(ledger stage) `(*state* data tokens)) `(define tokens (hash-table ,username ,,start-amt)))
                              ((record 'set!) (append '(ledger stage) `(*state* data times)) `(define times (hash-table ,username ,(*s7* 'cpu-time)))))
                        (let ((accounts-table ((ledger 'get) `(*state* data accounts) index))
                              (tokens-table ((ledger 'get) `(*state* data tokens) index))
                              (times-table ((ledger 'get) `(*state* data times) index))) 
                              (begin (eval (cadr accounts-table))
                                (eval (cadr tokens-table))
                                (eval (cadr times-table))
                                ; (format #f "table: ~a current pass: ~a" accounts (accounts username))
                                (if (eq? (accounts username) #f)
                                  (begin (set! (accounts username) password) 
                                        (set! (tokens username) ,start-amt)
                                        (set! (times username) (*s7* 'cpu-time))
                                        ((record 'set!) (append '(ledger stage) `(*state* data accounts)) `(define accounts ,accounts))
                                        ((record 'set!) (append '(ledger stage) `(*state* data tokens)) `(define tokens ,tokens))
                                        ((record 'set!) (append '(ledger stage) `(*state* data times)) `(define times ,times)))
                                  (error 'auth-error "This username already exists"))
                                (format #f "ACCOUNTS ~a TOKENS ~a TIMES ~a" accounts tokens times) ; don't show the private information
                                  )))))))

    

    (define contract-dep-debug
        `(lambda (record path src) 
            "Write the value to the path. Recursively generate parent
            directories if necessary.
            ; fix the below stuff 
            > record (fnc): library to access record commands
            > codepath (list sym|vec): path to the contract
            > src (exp|sync-pair): contract to be stored at the path
            < return (bool): boolean indicating success of the operation"
            (format #f "vars: ~a code: ~a codepath: ~a varspath: ~a" (cadr src) (append '(begin) (cddr src)) (append path '(code)) (append path '(vars)))))

    

    (define mysum `(lambda (num1 num2) (+ num1 num2)))

    (define mysum-call `(lambda (record num1 num2) (,mysum num1 num2)))

    (define test-vote `())

    
    ((record 'set!) '(control local contract-deploy) contract-deploy)
    ((record 'set!) '(control local mysum-call) mysum-call)
    ((record 'set!) '(control local contract-call) contract-call)
    ((record 'set!) '(control local contract-dep-debug) contract-dep-debug)
    ((record 'set!) '(control local create-account) create-account)
    ; ((record 'set!) '(control local contract-auth) contract-auth)
    )
)


