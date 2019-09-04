#lang racket
(require "../main.rkt"
         (for-syntax nanopass/base
                     syntax/name
                     syntax/id-set)
         racket/base)

(provide (except-out (all-from-out racket/base) #%module-begin))

(begin-for-syntax
  (define (demo prog)
    (define defined-variables (make-hasheqv))
    (define used-variables (make-hasheqv))
    
    (define (mark-defined! idt phase)
      (unless (or (eq? '_ (syntax-e idt))
                  (not (syntax-original? (syntax-local-introduce idt))))
        (unless (hash-ref defined-variables phase #f)
          (hash-set! defined-variables phase (mutable-free-id-set #:phase phase)))
        (free-id-set-add! (hash-ref defined-variables phase)
                          idt)))
    (define (mark-used! idt phase)
      (unless (hash-ref used-variables phase #f)
        (hash-set! used-variables phase (mutable-free-id-set #:phase phase)))
      (free-id-set-add! (hash-ref used-variables phase)
                        idt))
    
    (define (collect-ids! ids phase)
      (syntax-case ids ()
        [a (identifier? #'a) (mark-defined! #'a phase)]
        [() (void)]
        [(a . b) (mark-defined! #'a phase)
                 (collect-ids! #'b phase)]))
    
    (define-pass mark-unused-variable : (FullyExpandedProgram ModuleBeginForm) (prog) -> (FullyExpandedProgram ModuleBeginForm) ()
      (ModuleBeginForm : ModuleBeginForm (prog phase) -> ModuleBeginForm ()
                       [(#%plain-module-begin ,s ,[ml* 0 -> ml*] ...)
                        prog])
      (Expr : Expr (prog phase) -> Expr ()
            [,x 
             (mark-used! x phase)
             prog]
            [(#%top ,s ,x)
             (mark-used! x phase)
             prog]
            [(#%variable-reference ,s ,x)
             (mark-used! x phase)
             prog]
            [(#%variable-reference-top ,s ,x)
             (mark-used! x phase)
             prog]
            [(#%plain-lambda ,s ,fml ,[body*] ... ,[body])
             (collect-ids! fml phase)
             prog]
            [(case-lambda ,s [,fml ,[body*] ... ,[body]] ...)
             (for ([f (in-list fml)])
               (collect-ids! f phase))
             prog]
            [(let-values ,s ([(,x* ...) ,[e]] ...) ,[body*] ... ,[body])
             (for ([x (in-list x*)])
               (collect-ids! x phase))
             prog]
            [(letrec-values ,s ([(,x* ...) ,[e]] ...) ,[body*] ... ,[body])
             (for ([x (in-list x*)])
               (collect-ids! x phase))
             prog])
      (GeneralTopLevelForm : GeneralTopLevelForm (prog phase) -> GeneralTopLevelForm ()
                           #;[(define-values ,s (,x ...) ,[e])
                              (collect-ids! x phase)
                              prog
                              ] ;;ignore module level definitions
                           [(define-syntaxes ,s (,x ...) ,[e0 (+ phase 1) -> e1])
                            prog])
      (ModuleLevelForm : ModuleLevelForm (prog phase) -> ModuleLevelForm ()
                       [(begin-for-syntax ,s ,[ml* (+ phase 1) -> ml^] ...)
                        prog])
      (SubModuleForm : SubModuleForm (prog phase) -> SubModuleForm ())
      (ModuleBeginForm prog 0))
    (mark-unused-variable prog)
    (define unused-set (apply append
                              (for/list
                                  ([(ph vars) (in-hash defined-variables)])
                                (define same-phase-used
                                  (hash-ref used-variables ph #f))
                                (when same-phase-used
                                  (free-id-set-subtract! vars same-phase-used))
                                (free-id-set->list vars))))

    (unless (null? unused-set)
      ((error-display-handler)
       "warning : unused variables"
       (make-exn:fail:syntax "" (current-continuation-marks) unused-set))
      #;
      (raise-syntax-error 'unused-variable-warner
                          "warning : unused variables"
                          #f
                          #f
                          unused-set))
    prog))

(define/provide-module-begin* demo)