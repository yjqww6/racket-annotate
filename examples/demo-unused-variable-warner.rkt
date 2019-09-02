#lang racket
(require "../main.rkt"
         (for-syntax nanopass/base
                     syntax/name
                     syntax/id-set)
         racket/base)

(provide (except-out (all-from-out racket/base) #%module-begin))
(begin-for-syntax
  (define (demo prog)
    (define defined-variables '())
    (define used-variables '())
    (define (mark-defined! idt)
      (set! defined-variables (cons idt defined-variables)))
    (define (mark-used! idt)
      (set! used-variables (cons idt used-variables)))
    (define (collect-ids! ids)
      (syntax-case ids ()
        [() (void)]
        [(a . b) (mark-defined! #'a)
                 (collect-ids! #'b)]
        [(a) (mark-defined! #'a)]))
    (define-pass mark-unused-variable : (FullyExpandedProgram ModuleBeginForm) (prog) -> (FullyExpandedProgram ModuleBeginForm) ()
      (ModuleBeginForm : ModuleBeginForm (prog) -> ModuleBeginForm (prog)
                       [(#%plain-module-begin ,s ,[ml*] ... ,[ml])
                        prog])
      (Expr : Expr (prog) -> Expr ()
            [,x 
             (mark-used! x)
             prog]
            [(#%top ,s ,x)
             (mark-used! x)
             prog]
            [(#%variable-reference ,s ,x)
             (mark-used! x)
             prog]
            [(#%variable-reference-top ,s ,x)
             (mark-used! x)
             prog]
            [(#%plain-lambda ,s ,fml ,[body*] ... ,[body])
             (collect-ids! fml)
             prog]
            [(case-lambda ,s [,fml ,[body*] ... ,[body]] ...)
             (for ([f fml])
               (collect-ids! f))
             prog]
            [(let-values ,s ([(,x* ...) ,[e]] ...) ,[body*] ... ,[body])
             (for ([x x*])
               (collect-ids! x))
             prog]
            [(letrec-values ,s ([(,x* ...) ,[e]] ...) ,[body*] ... ,[body])
             (for ([x x*])
               (collect-ids! x))
             prog]
            )
      (GeneralTopLevelForm : GeneralTopLevelForm (prog) -> GeneralTopLevelForm ()
                           [(define-values ,s (,x ...) ,[e])
                            (collect-ids! x)
                            prog
                            ]
                           )
      )
    (mark-unused-variable prog)
    (define unused-set 
      (filter (λ (x)
                (and (not (member x used-variables free-identifier=?))
                     (not (eq? '_ (syntax-e x))) ;; _ should not be considered as unused vars
                     ))
              defined-variables)
      )
    (unless (null? unused-set)
      (error "Warning : unused variables :" unused-set))
    prog
    )
  )

(define/provide-module-begin* demo)