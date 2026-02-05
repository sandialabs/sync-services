(macro (secret-1 secret-2 window control standard chain tree configuration ledger . classes)

  ;; install control logic
  (sync-call `(,control ,secret-1) #t)

  (define (call function)
    (sync-call `(*call* ,secret-1 ,function) #t))

  (define (set-query function)
    (sync-call `(*set-query* ,secret-1 ,function) #t))

  ;; install standard library
  (call `(,standard '(control class standard) '(control object standard)))

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

  ; define secret store
  (call `(lambda (root)
           ((root 'set!) '(interface secret) (sync-hash (expression->byte-vector ,secret-2)))))

  ;; set query logic
  (set-query '(lambda (root query)
                (let ((func (assoc 'function query))
                      (args (assoc 'arguments query))
                      (auth (assoc 'authentication query))
                      (public '(size synchronize resolve information)))
                  (if (and (not (memq (cadr func) public))
                           (not (equal? (sync-hash (expression->byte-vector (cadr auth)))
                                        ((root 'get) '(interface secret)))))
                      (error 'authentication-error "Could not authenticate restricted interface call"))
                  (case (cadr func)
                    ((*secret*) (lambda (root secret-old secret-new)
                                  (if (not (equal? (sync-hash (expression->byte-vector secret-old)) ((root 'get) '(interface secret))))
                                      (error 'authentication-error "Could not authenticate secret update"))
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
                    (else 
                     (let* ((node ((root 'get) '(control object ledger)))
                            (ledger ((eval (byte-vector->expression (sync-car node))) node))
                            (result (apply (ledger (cadr func)) (if args (cadr args) '()))))
                       ((root 'set!) '(control object ledger) (ledger)) result))))))

  ;; ;; define secret store and endpoint
  ;; (call `(lambda (root)
  ;;          ((root 'set!) '(interface secret) (sync-hash (expression->byte-vector ,secret-2)))
  ;;          ((root 'set!) '(control endpoint *secret*)
  ;;           '(lambda (root secret-old secret-new)
  ;;              (if (not (equal? (sync-hash (expression->byte-vector secret-old)) ((root 'get) '(interface secret))))
  ;;                  (error 'authentication-error "Could not authenticate secret update"))
  ;;              ((root 'set!) '(interface secret) (sync-hash (expression->byte-vector secret-new)))))))

  ;; (call '(lambda (root)
  ;;          ((root 'set!) '(control endpoint step)
  ;;           '(lambda (root secret)
  ;;              (if (not (equal? (sync-hash (expression->byte-vector secret)) ((root 'get) '(interface secret))))
  ;;                  (error 'authentication-error "Could not authenticate step call"))
  ;;              (let* ((node ((root 'get) '(control object ledger)))
  ;;                     (ledger ((eval (byte-vector->expression (sync-car node))) node)))
  ;;                (let loop ((sub-steps ((ledger 'step-generate))))
  ;;                  (if (null? sub-steps) ((ledger 'size))
  ;;                      (begin (sync-call `(private ,secret ((ledger ',(caar sub-steps)) ,@(cdar sub-steps))) #f)
  ;;                             (loop (cdr sub-steps))))))))))

  ;; ;; define authenticated endpoint (e.g.,: (private "password" method 
  ;; (call '(lambda (root)
  ;;          ((root 'set!) '(control endpoint private)
  ;;           '(lambda (root secret expression)
  ;;              (if (not (equal? (sync-hash (expression->byte-vector secret)) ((root 'get) '(interface secret))))
  ;;                  (error 'authentication-error "Could not authenticate private call"))
  ;;              (case (caar expression)
  ;;                ((ledger)
  ;;                 (let* ((node ((root 'get) '(control object ledger)))
  ;;                        (ledger ((eval (byte-vector->expression (sync-car node))) node))
  ;;                        (result (apply (ledger (cadr (cadar expression))) (cdr expression))))
  ;;                   ((root 'set!) '(control object ledger) (ledger)) result))
  ;;                (else (error 'interface-error "Interaction with specified object not supported")))))))

  ;; ;; define non-authenticated endpoint
  ;; (call '(lambda (root)
  ;;          ((root 'set!) '(control endpoint public)
  ;;           `(lambda (root expression)
  ;;              (case (caar expression)
  ;;                ((ledger)
  ;;                 (let ((method (cadr (cadar expression))))
  ;;                   (case method
  ;;                     ((size synchronize resolve information)
  ;;                      (let* ((node ((root 'get) '(control object ledger)))
  ;;                             (ledger ((eval (byte-vector->expression (sync-car node))) node))
  ;;                             (result (apply (ledger method) (cdr expression))))
  ;;                        ((root 'set!) '(control object ledger) (ledger)) result))
  ;;                     (else (error 'interface-error "Interaction with specified method not supported")))))
  ;;                (else (error 'interface-error "Interaction with specified object not supported")))))))

  "Installed base interface")
