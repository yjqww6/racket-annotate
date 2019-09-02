#lang racket/base
(require (for-syntax racket/base) (for-template racket/base)
         nanopass/base "implicit.rkt"
         syntax/kerncase syntax/stx racket/contract/base
         racket/contract/region)

(provide (all-defined-out))

(define-language FullyExpandedProgram
  (terminals
   [identifier (x id)]
   [syntax (s d fml)])
  
  (Expr
   (e body)
   x
   (#%expression s e)
   (#%plain-lambda s fml body* ... body)
   (case-lambda s [fml body* ... body] ...)
   (if s e0 e1 e2)
   (begin s body* ... body)
   (begin0 s body body* ...)
   (let-values s ([(x* ...) e] ...) body* ... body)
   (letrec-values s ([(x* ...) e] ...) body* ... body)
   (set! s x e)
   (quote s d)
   (quote-syntax s d)
   (quote-syntax-local s d)
   (with-continuation-mark s e0 e1 e2)
   (#%plain-app s e e* ...)
   (#%top s x)
   (#%variable-reference s x)
   (#%variable-reference-top s x)
   (#%variable-reference-null s))
  
  (GeneralTopLevelForm
   (gtl)
   e
   (define-values s (x* ...) e)
   (define-syntaxes s (x* ...) e)
   (#%require s d))

  (SubModuleForm
   (sm)
   (module s0 s1 id d ml* ...)
   (module* s0 s1 id d ml* ...))
  
  (ModuleLevelForm
   (ml)
   gtl
   (#%provide s d)
   (begin-for-syntax s ml* ...)
   sm
   (#%declare s d))
  
  (TopLevelForm
   (tl)
   gtl
   (tl:#%expression s e)
   (tl:module s0 s1 id d ml* ...)
   (tl:begin s tl* ...)
   (tl:begin-for-syntax s tl* ...)
   )
  )

(define/contract
  (FullyExpandedProgram->syntax
   prog [start 'top-level-form] [base-phase (syntax-local-phase-level)])
  (->* (FullyExpandedProgram?)
       ((or/c 'top-level-form 'module-level-form 'expr)
        exact-integer?)
       syntax?)
  
  (define-pass fep->stx
    : FullyExpandedProgram (prog start) -> * (ss)
    (definitions
      (define (d->s k s)
        (syntax-rearm
         (datum->syntax (syntax-disarm k #f) s)
         k))
      (define-syntax (lit stx)
        (syntax-case stx ()
          [(_ id)
           #`(syntax-shift-phase-level
              #'id
              #,(datum->syntax stx 'phase))])))
    (Expr
     : Expr (prog phase) -> * (ss)
     [,x x]
     [(#%expression ,s ,[e])
      (d->s s `(,(lit #%expression) ,e))]
     [(#%plain-lambda ,s ,fml ,[body*] ... ,[body])
      (d->s s `(,(lit #%plain-lambda) ,fml ,@body* ,body))]
     [(case-lambda ,s [,fml ,[body*] ... ,[body]] ...)
      (d->s
       s
       `(,(lit case-lambda) ,@(for/list ([fml (in-list fml)]
                                         [body (in-list body)]
                                         [body* (in-list body*)])
                                `(,fml ,@body* ,body))))]
     [(if ,s ,[e0] ,[e1] ,[e2])
      (d->s s `(,(lit if) ,e0 ,e1 ,e2))]
     [(begin ,s ,[body*] ... ,[body])
      (d->s s `(,(lit begin) ,@body* ,body))]
     [(begin0 ,s ,[body] ,[body*] ...)
      (d->s s `(,(lit begin0) ,body ,@body*))]
     [(let-values ,s ([(,x* ...) ,[e]] ...) ,[body*] ... ,[body])
      (d->s
       s
       `(,(lit let-values) ,(for/list ([x* (in-list x*)]
                                       [e (in-list e)])
                              `[,x* ,e])
                           ,@body* ,body))]
     [(letrec-values ,s ([(,x* ...) ,[e]] ...) ,[body*] ... ,[body])
      (d->s
       s
       `(,(lit letrec-values) ,(for/list ([x* (in-list x*)]
                                          [e (in-list e)])
                                 `[,x* ,e])
                              ,@body* ,body))]
     [(set! ,s ,x ,[e])
      (d->s s `(,(lit set!) ,x ,e))]
     [(quote ,s ,d)
      (d->s s `(,(lit quote) ,d))]
     [(quote-syntax ,s ,d)
      (d->s s `(,(lit quote-syntax) ,d))]
     [(quote-syntax-local ,s ,d)
      (d->s s `(,(lit quote-syntax) ,d #:local))]
     [(with-continuation-mark ,s ,[e0] ,[e1] ,[e2])
      (d->s s `(,(lit with-continuation-mark) ,e0 ,e1 ,e2))]
     [(#%plain-app ,s ,[e] ,[e*] ...)
      (d->s s `(,(lit #%plain-app) ,e ,@e*))]
     [(#%top ,s ,x)
      (d->s s `(,(lit #%top) . ,x))]
     [(#%variable-reference ,s ,x)
      (d->s s `(,(lit #%variable-reference) ,x))]
     [(#%variable-reference-top ,s ,x)
      (d->s s `(,(lit #%variable-reference)
                (,(lit #%top) ,x)))]
     [(#%variable-reference-null ,s)
      (d->s s `(,(lit #%variable-reference)))])
      
    (GeneralTopLevelForm
     : GeneralTopLevelForm (prog phase) -> * (ss)
     [,e (Expr e phase)]
     [(define-values ,s (,x* ...) ,[e])
      (d->s s `(,(lit define-values) ,x* ,e))]
     [(define-syntaxes ,s (,x* ...) ,[e (+ 1 phase) -> e])
      (d->s s `(,(lit define-syntaxes) ,x* ,e))]
     [(#%require ,s ,d)
      (d->s s `(,(lit #%require) . ,d))])

    (SubModuleForm
     : SubModuleForm (prog phase) -> * (ss)
     [(module ,s0 ,s1 ,id ,d ,[ml* 0 -> ml*] ...)
      (d->s
       s0
       `(,(lit module) ,id ,d
                       ,(d->s
                         s1
                         `(,(let ([phase 0]) (lit #%plain-module-begin))
                           ,@ml*))))]
     [(module* ,s0 ,s1 ,id ,d ,[ml* 0 -> ml*] ...)
      (d->s
       s0
       `(,(lit module*) ,id ,d
                        ,(d->s
                          s1
                          `(,(let ([phase 0]) (lit #%plain-module-begin))
                            ,@ml*))))])
  
    (ModuleLevelForm
     : ModuleLevelForm (prog phase) -> * (ss)
     [,gtl (GeneralTopLevelForm gtl phase)]
     [(#%provide ,s ,d)
      (d->s s `(,(lit #%provide) . ,d))]
     [(begin-for-syntax ,s ,[ml* (+ 1 phase) -> ml*] ...)
      (d->s s `(,(lit begin-for-syntax) ,@ml*))]
     [,sm (SubModuleForm sm phase)]
     [(#%declare ,s ,d)
      (d->s s `(,(lit #%declare) . ,d))])
  
    (TopLevelForm
     : TopLevelForm (prog phase) -> * (ss)
     [,gtl (GeneralTopLevelForm gtl phase)]
     [(tl:#%expression ,s ,[e])
      (d->s s `(,(lit #%expression) ,e))]
     [(tl:module ,s0 ,s1 ,id ,d ,[ml* 0 -> ml*] ...)
      (d->s
       s0
       `(,(lit module) ,id ,d
                       ,(d->s
                         s1
                         `(,(lit #%plain-module-begin)
                           ,@ml*))))]
     [(tl:begin ,s ,[tl*] ...)
      (d->s s `(,(lit begin) ,@tl*))]
     [(tl:begin-for-syntax ,s ,[tl* (+ 1 phase) -> tl*] ...)
      (d->s s `(,(lit begin-for-syntax) ,@tl*))]
     )
    (case start
      [(top-level-form)
       (TopLevelForm prog base-phase)]
      [(module-level-form)
       (ModuleLevelForm prog base-phase)]
      [(expr)
       (Expr prog base-phase)])
    )
  (fep->stx prog start))

(define/contract
  (syntax->FullyExpandedProgram
   stx [start 'top-level-form] [base-phase (syntax-local-phase-level)])
  (->* (syntax?)
       ((or/c 'top-level-form 'module-level-form 'expr)
        exact-integer?)
       FullyExpandedProgram?)
  
  (define-implicit-key phase)

  (define-syntax-rule (phase-map proc new-phase arg)
    (stx-map (with-implicit ([phase new-phase])
               proc)
             arg))
  
  (define-implicit (top-level-form stx) (phase)
    (with-output-language (FullyExpandedProgram TopLevelForm)
      (kernel-syntax-case/phase
       (syntax-disarm stx #f) phase
       [(#%expression e) `(tl:#%expression ,stx ,(expr #'e))]
       [(module id module-path p)
        (kernel-syntax-case/phase
         #'p 0
         [(#%plain-module-begin ml ...)
          `(tl:module ,stx ,#'p ,#'id ,#'module-path
                      ,(phase-map module-level-form 0 #'(ml ...)) ...)])]
       [(begin tl ...)
        `(tl:begin ,stx
                   ,(stx-map top-level-form #'(tl ...)) ...)]
       [(begin-for-syntax tl ...)
        `(tl:begin ,stx
                   ,(phase-map top-level-form (+ 1 phase) #'(tl ...))
                   ...)]
       [_ (general-top-level-form stx)])))

  (define-implicit (module-level-form stx) (phase)
    (with-output-language (FullyExpandedProgram ModuleLevelForm)
      (kernel-syntax-case/phase
       (syntax-disarm stx #f) phase
       [(#%provide d ...)
        `(#%provide ,stx ,#'(d ...))]
       [(begin-for-syntax f ...)
        `(begin-for-syntax
           ,stx
           ,(phase-map module-level-form (+ 1 phase) #'(f ...)))]
       [(#%declare kw ...)
        `(#%declare ,stx ,#'(kw ...))]
       [_
        (with-output-language (FullyExpandedProgram SubModuleForm)
          (kernel-syntax-case/phase
           stx phase
           [(module id module-path p)
            (kernel-syntax-case/phase
             (syntax-disarm #'p #f) 0
             [(#%plain-module-begin ml ...)
              `(module ,stx ,#'p ,#'id ,#'module-path
                 ,(phase-map module-level-form 0 #'(ml ...))
                 ...)])]
           [(module* id module-path p)
            (kernel-syntax-case/phase
             (syntax-disarm #'p #f) 0
             [(#%plain-module-begin ml ...)
              `(module* ,stx ,#'p ,#'id ,#'module-path
                 ,(phase-map module-level-form 0 #'(ml ...))
                 ...)])]
           [_ (general-top-level-form stx)]))])))

  (define-implicit (general-top-level-form stx) (phase)
    (with-output-language (FullyExpandedProgram GeneralTopLevelForm)
      (kernel-syntax-case/phase
       (syntax-disarm stx #f) phase
       [(define-values (id ...) e)
        `(define-values ,stx (,(syntax->list #'(id ...)) ...)
           ,(expr #'e))]
       [(define-syntaxes (id ...) e)
        `(define-syntaxes ,stx (,(syntax->list #'(id ...)) ...)
           ,(expr #'e #:phase (+ 1 phase)))]
       [(#%require raw-require-spec ...)
        `(#%require ,stx ,#'(raw-require-spec ...))]
       [_ (expr stx)])))

  (define-implicit (expr stx) (phase)
    (with-output-language (FullyExpandedProgram Expr)
      (kernel-syntax-case/phase
       (syntax-disarm stx #f) phase
       [x (identifier? #'x) #'x]
       [(#%expression e)
        `(#%expression ,stx ,(expr #'e))]
       [(#%plain-lambda fmls e^ ... e)
        `(#%plain-lambda ,stx ,#'fmls
                         ,(stx-map expr #'(e^ ...)) ...
                         ,(expr #'e))]
       [(case-lambda (fmls e^ ... e) ...)
        `(case-lambda ,stx
                      [,(syntax->list #'(fmls ...))
                       ,(stx-map (λ (b) (stx-map expr b))
                                 #'((e^ ...) ...))
                       ...
                       ,(stx-map expr #'(e ...))]
                      ...)]
       [(if e0 e1 e2)
        `(if ,stx ,(expr #'e0)
             ,(expr #'e1)
             ,(expr #'e2))]
       [(begin e^ ... e)
        `(begin ,stx ,(stx-map expr #'(e^ ...)) ...
                ,(expr #'e))]
       [(begin0 e e^ ...)
        `(begin0 ,stx ,(expr #'e)
                 ,(stx-map expr #'(e^ ...)) ...)]
       [(let-values ([(id ...) e] ...)
          b^ ... b)
        `(let-values ,stx ([(,(stx-map syntax->list #'((id ...) ...)) ...)
                            ,(stx-map expr #'(e ...))] ...)
           ,(stx-map expr #'(b^ ...)) ...
           ,(expr #'b))]
       [(letrec-values ([(id ...) e] ...)
          b^ ... b)
        `(letrec-values ,stx ([(,(stx-map syntax->list #'((id ...) ...)) ...)
                               ,(stx-map expr #'(e ...))] ...)
           ,(stx-map expr #'(b^ ...)) ...
           ,(expr #'b))]
       [(set! id e)
        `(set! ,stx ,#'id ,(expr #'e))]
       [(quote d)
        `(quote ,stx ,#'d)]
       [(quote-syntax d)
        `(quote-syntax ,stx ,#'d)]
       [(quote-syntax d #:local)
        `(quote-syntax-local ,stx ,#'d)]
       [(with-continuation-mark e0 e1 e2)
        `(with-continuation-mark ,stx ,(expr #'e0)
           ,(expr #'e1) ,(expr #'e2))]
       [(#%plain-app e^ e ...)
        `(#%plain-app ,stx ,(expr #'e^)
                      ,(stx-map expr #'(e ...)) ...)]
       [(#%top . id)
        `(#%top ,stx ,#'id)]
       [(#%variable-reference id)
        `(#%variable-reference ,stx ,#'id)]
       [(#%variable-reference (#%top . id))
        `(#%variable-reference-top ,stx ,#'id)]
       [(#%variable-reference)
        `(#%variable-reference-null ,stx)])))
  (with-implicit ([phase base-phase])
    (case start
      [(top-level-form)
       (top-level-form stx)]
      [(module-level-form)
       (module-level-form stx)]
      [(expr)
       (expr stx)])))