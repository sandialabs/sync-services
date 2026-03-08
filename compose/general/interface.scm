(macro (secret-1 secret-2 window control standard chain tree configuration ledger . classes)

  ;; install control logic
  (sync-call `(,control ,secret-1) #t)

  (define (call function)
    (sync-call `(*call* ,secret-1 ,function) #t))

  (define (set-query function)
    (sync-call `(*set-query* ,secret-1 ,function) #t))

  ;; install and instantiate standard library
  (call `(lambda (root)
           (let ((init (caddr ,standard)))
             ((root 'set!) '(control class standard) ,standard)
             ((root 'set!) '(control object standard)
              (((eval `(lambda* ,(cddadr init) ,@(cddr init))) ,standard))))))

  ;; install required classes
  (call `(lambda (root) ((root 'set!) '(control class chain) ,chain)))
  (call `(lambda (root) ((root 'set!) '(control class tree) ,tree)))
  (call `(lambda (root) ((root 'set!) '(control class configuration) ,configuration)))
  (call `(lambda (root) ((root 'set!) '(control class ledger) ,ledger)))

  ;; install optional classes
  (let loop ((classes classes))
    (if (null? classes) #t
        (begin (call `(lambda (root ((root 'set!) '(control object ,(caar classes)) ,(cadadr classes)))))
               (loop (cdr classes)))))

  ;; instantiate ledger
  (call `(lambda (root)
           (let* ((std-node ((root 'get) '(control object standard)))
                  (standard ((eval (byte-vector->expression (sync-car std-node))) std-node))
                  (config-class ((root 'get) '(control class configuration)))
                  (tree-class ((root 'get) '(control class tree)))
                  (chain-class ((root 'get) '(control class chain)))
                  (ledger-class ((root 'get) '(control class ledger)))
                  (keys (crypto-generate (expression->byte-vector ,secret-2)))
                  (config-expr `((public ((window ,,window)
                                          (public-key ,(car keys))))
                                 (private ((secret-key ,(cdr keys))
                                           (tree-class ,tree-class)
                                           (chain-class ,chain-class)))))
                  (config ((standard 'make) config-class `(,config-expr)))
                  (ledger ((standard 'make) ledger-class `(,standard ,config))))
             ((root 'set!) '(control object ledger) (ledger)))))

  ;; define secret store
  (call `(lambda (root)
           ((root 'set!) '(interface secret) (sync-hash (expression->byte-vector ,secret-2)))))

  ;; set query logic
  (set-query
   '(lambda (root query)
      (if (equal? query '(*api*)) ,api-help
          (let ((func (assoc 'function query))
                (args (assoc 'arguments query))
                (auth (assoc 'authentication query))
                (public '(size synchronize resolve information))
                (restricted '(configuration peers set! get pin! unpin! general-peer!
                                            step-chain! step-peer! *secret* *step*)))
            (if (not (or (memq (cadr func) public) (memq (cadr func) restricted)))
                (error 'function-error "Function not recognized by general interface"))
            (if (and (not (memq (cadr func) public))
                     (not (equal? (sync-hash (expression->byte-vector (cadr auth)))
                                  ((root 'get) '(interface secret)))))
                (error 'authentication-error "Could not authenticate restricted interface call"))
            (case (cadr func)
              ((*secret*)
               (let ((secret-new (cadr (assoc 'secret (cadr args)))))
                 ((root 'set!) '(interface secret) (sync-hash (expression->byte-vector secret-new)))))
              ((*step*)
               (let* ((node ((root 'get) '(control object ledger)))
                      (ledger ((eval (byte-vector->expression (sync-car node))) node)))
                 (let loop ((sub-steps ((ledger 'step-generate))))
                   (if (null? sub-steps) ((ledger 'size))
                       (begin (sync-call `((function ,(caar sub-steps))
                                           (authentication ,(cadr auth))
                                           (arguments ,(cdar sub-steps))) #f)
                              (loop (cdr sub-steps)))))))
              ((general-peer!)
               (let* ((node ((root 'get) '(control object ledger)))
                      (ledger ((eval (byte-vector->expression (sync-car node))) node))
                      (name (cadr (assoc 'name (cadr args))))
                      (interface (cadr (assoc 'interface (cadr args))))
                      (result ((ledger 'peer!) name
                               `((information (lambda ()
                                                (sync-remote ,interface
                                                             '((function information)))))
                                 (synchronize (lambda (index)
                                                (sync-remote ,interface
                                                             `((function synchronize)
                                                               (arguments ((index ,index)))))))
                                 (resolve (lambda (source target)
                                            (sync-remote ,interface
                                                         `((function resolve)
                                                           (arguments ((index ,source) (path ,target)))))))))))
                 ((root 'set!) '(control object ledger) (ledger)) result))
              (else 
               (let* ((node ((root 'get) '(control object ledger)))
                      (ledger ((eval (byte-vector->expression (sync-car node))) node))
                      (flat (let loop ((in (if args (reverse (cadr args)) '())) (out '()))
                              (if (null? in) out
                                  (loop (cdr in) (append `(,(symbol->keyword (caar in)) ,(cadar in)) out)))))
                      (result (apply (ledger (cadr func)) flat)))
                 ((root 'set!) '(control object ledger) (ledger)) result)))))))

  "Installed base interface")
