#lang racket/base
(require (for-syntax racket/base)
         nanopass/base
         syntax/kerncase syntax/stx)

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
   (#%expression s e)
   (tl:module s0 s1 id d ml* ...)
   (tl:begin s tl* ...)
   (tl:begin-for-syntax s tl* ...)
   )
  )

;;;TODO: rearm 
(define (FullyExpandedProgram->syntax fpe [start 'top-level-form]
                                      [shift-literal -1])
  (define-pass FullyExpandedProgram->syntax
    : FullyExpandedProgram (fpe start) -> * (ss)
    (definitions
      (define-syntax (lit stx)
        (syntax-case stx ()
          [(_ id)
           #`(syntax-shift-phase-level
              #'id
              (+ shift-literal #,(datum->syntax stx 'phase)))])))
    (Expr
     : Expr (fpe phase) -> * (ss)
     [,x x]
     [(#%expression ,s ,[e])
      (datum->syntax s `(,(lit #%expression) ,e))]
     [(#%plain-lambda ,s ,fml ,[body*] ... ,[body])
      (datum->syntax s `(,(lit #%plain-lambda) ,fml ,@body* ,body))]
     [(case-lambda ,s [,fml ,[body*] ... ,[body]] ...)
      (datum->syntax
       s
       `(,(lit case-lambda) ,@(for/list ([fml (in-list fml)]
                                         [body (in-list body)]
                                         [body* (in-list body*)])
                                `(,fml ,@body* ,body))))]
     [(if ,s ,[e0] ,[e1] ,[e2])
      (datum->syntax s `(,(lit if) ,e0 ,e1 ,e2))]
     [(begin ,s ,[body*] ... ,[body])
      (datum->syntax s `(,(lit begin) ,@body* ,body))]
     [(begin0 ,s ,[body] ,[body*] ...)
      (datum->syntax s `(,(lit begin0) ,body ,@body*))]
     [(let-values ,s ([(,x* ...) ,[e]] ...) ,[body*] ... ,[body])
      (datum->syntax
       s
       `(,(lit let-values) ,(for/list ([x* (in-list x*)]
                                       [e (in-list e)])
                              `[,x* ,e])
                           ,@body* ,body))]
     [(letrec-values ,s ([(,x* ...) ,[e]] ...) ,[body*] ... ,[body])
      (datum->syntax
       s
       `(,(lit letrec-values) ,(for/list ([x* (in-list x*)]
                                          [e (in-list e)])
                                 `[,x* ,e])
                              ,@body* ,body))]
     [(set! ,s ,x ,[e])
      (datum->syntax s `(,(lit set!) ,x ,e))]
     [(quote ,s ,d)
      (datum->syntax s `(,(lit quote) ,d))]
     [(quote-syntax ,s ,d)
      (datum->syntax s `(,(lit quote-syntax) ,d))]
     [(quote-syntax-local ,s ,d)
      (datum->syntax s `(,(lit quote-syntax) ,d #:local))]
     [(with-continuation-mark ,s ,[e0] ,[e1] ,[e2])
      (datum->syntax s `(,(lit with-continuation-mark) ,e0 ,e1 ,e2))]
     [(#%plain-app ,s ,[e] ,[e*] ...)
      (datum->syntax s `(,(lit #%plain-app) ,e ,@e*))]
     [(#%top ,s ,x)
      (datum->syntax s `(,(lit #%top) . ,x))]
     [(#%variable-reference ,s ,x)
      (datum->syntax s `(,(lit #%variable-reference) ,x))]
     [(#%variable-reference-top ,s ,x)
      (datum->syntax s `(,(lit #%variable-reference)
                         (,(lit #%top) ,x)))]
     [(#%variable-reference-null ,s)
      (datum->syntax s `(,(lit #%variable-reference)))])
      
    (GeneralTopLevelForm
     : GeneralTopLevelForm (fpe phase) -> * (ss)
     [,e (Expr e phase)]
     [(define-values ,s (,x* ...) ,[e])
      (datum->syntax s `(,(lit define-values) ,x* ,e))]
     [(define-syntaxes ,s (,x* ...) ,[e (+ 1 phase) -> e])
      (datum->syntax s `(,(lit define-syntaxes) ,x* ,e))]
     [(#%require ,s ,d)
      (datum->syntax s `(,(lit #%require) . ,d))])

    (SubModuleForm
     : SubModuleForm (fpe phase) -> * (ss)
     [(module ,s0 ,s1 ,id ,d ,[ml* 0 -> ml*] ...)
      (datum->syntax
       s0
       `(,(lit module) ,id ,d
                       ,(datum->syntax
                         s1
                         `(,(let ([phase 0]) (lit #%plain-module-begin))
                           ,@ml*))))]
     [(module* ,s0 ,s1 ,id ,d ,[ml* 0 -> ml*] ...)
      (datum->syntax
       s0
       `(,(lit module*) ,id ,d
                        ,(datum->syntax
                          s1
                          `(,(let ([phase 0]) (lit #%plain-module-begin))
                            ,@ml*))))])
  
    (ModuleLevelForm
     : ModuleLevelForm (fpe phase) -> * (ss)
     [,gtl (GeneralTopLevelForm gtl phase)]
     [(#%provide ,s ,d)
      (datum->syntax s `(,(lit #%provide) . ,d))]
     [(begin-for-syntax ,s ,[ml* (+ 1 phase) -> ml*] ...)
      (datum->syntax s `(,(lit begin-for-syntax) ,@ml*))]
     [,sm (SubModuleForm sm phase)]
     [(#%declare ,s ,d)
      (datum->syntax s `(,(lit #%declare) . ,d))])
  
    (TopLevelForm
     : TopLevelForm (fpe phase) -> * (ss)
     [,gtl (GeneralTopLevelForm gtl phase)]
     [(#%expression ,s ,[e])
      (datum->syntax s `(,(lit #%expression) ,e))]
     [(tl:module ,s0 ,s1 ,id ,d ,[ml* 0 -> ml*] ...)
      (datum->syntax
       s0
       `(,(lit module) ,id ,d
                       ,(datum->syntax
                         s1
                         `(,(lit #%plain-module-begin)
                           ,@ml*))))]
     [(tl:begin ,s ,[tl*] ...)
      (datum->syntax s `(,(lit begin) ,@tl*))]
     [(tl:begin-for-syntax ,s ,[tl* (+ 1 phase) -> tl*] ...)
      (datum->syntax s `(,(lit begin-for-syntax) ,@tl*))]
     )
    (case start
      [(top-level-form)
       (TopLevelForm fpe 0)]
      [(module-level-form)
       (ModuleLevelForm fpe 0)]
      [(expr)
       (Expr fpe 0)])
    )
  (FullyExpandedProgram->syntax fpe start))

(define (syntax->FullyExpandedProgram stx [start 'top-level-form] [phase 0])
  
  (define ((top-level-form phase) stx)
    (with-output-language (FullyExpandedProgram TopLevelForm)
      (kernel-syntax-case/phase
       stx phase
       [(#%expression e) `(#%expression ,stx ,((expr 0) #'e))]
       [(module id module-path p)
        (kernel-syntax-case/phase
         #'p 0
         [(#%plain-module-begin ml ...)
          `(tl:module ,stx ,#'p ,#'id ,#'module-path
                      ,(stx-map (module-level-form 0)
                                #'(ml ...)) ...)])]
       [(begin tl ...)
        `(tl:begin ,stx
                   ,(stx-map (top-level-form phase) #'(tl ...)) ...)]
       [(begin-for-syntax tl ...)
        `(tl:begin ,stx
                   ,(stx-map (top-level-form (+ 1 phase))
                             #'(tl ...)) ...)]
       [_ ((general-top-level-form phase) stx)])))

  (define ((module-level-form phase) stx)
    (with-output-language (FullyExpandedProgram ModuleLevelForm)
      (let ([stx (syntax-disarm stx #f)])
        (kernel-syntax-case/phase
         stx phase
         [(#%provide d ...)
          `(#%provide ,stx ,#'(d ...))]
         [(begin-for-syntax f ...)
          `(begin-for-syntax
             ,stx
             ,(stx-map (module-level-form (+ 1 phase)) #'(f ...)))]
         [(#%declare kw ...)
          `(#%declare ,stx ,#'(kw ...))]
         [_
          (with-output-language (FullyExpandedProgram SubModuleForm)
            (kernel-syntax-case/phase
             stx phase
             [(module id module-path p)
              (let ([p (syntax-disarm #'p #f)])
                (kernel-syntax-case/phase
                 p 0
                 [(#%plain-module-begin ml ...)
                  `(module ,stx ,p ,#'id ,#'module-path
                     ,(stx-map (module-level-form 0)
                               #'(ml ...)) ...)]))]
             [(module* id module-path p)
              (let ([p (syntax-disarm #'p #f)])
                (kernel-syntax-case/phase
                 p 0
                 [(#%plain-module-begin ml ...)
                  `(module* ,stx ,p ,#'id ,#'module-path
                     ,(stx-map (module-level-form 0)
                               #'(ml ...)) ...)]))]
             [_ ((general-top-level-form phase) stx)]))]))))

  (define ((general-top-level-form phase) stx)
    (with-output-language (FullyExpandedProgram GeneralTopLevelForm)
      (let ([stx (syntax-disarm stx #f)])
        (kernel-syntax-case/phase
         stx phase
         [(define-values (id ...) e)
          `(define-values ,stx (,(syntax->list #'(id ...)) ...)
             ,((expr phase) #'e))]
         [(define-syntaxes (id ...) e)
          `(define-syntaxes ,stx (,(syntax->list #'(id ...)) ...)
             ,((expr (+ 1 phase)) #'e))]
         [(#%require raw-require-spec ...)
          `(#%require ,stx ,#'(raw-require-spec ...))]
         [_ ((expr phase) stx)]))))

  (define ((expr phase) stx)
    (with-output-language (FullyExpandedProgram Expr)
      (let ([stx (syntax-disarm stx #f)])
        (kernel-syntax-case/phase
         stx phase
         [x (identifier? #'x) #'x]
         [(#%expression e)
          `(#%expression ,stx ,((expr phase) #'e))]
         [(#%plain-lambda fmls e^ ... e)
          `(#%plain-lambda ,stx ,#'fmls
                           ,(stx-map (expr phase) #'(e^ ...)) ...
                           ,((expr phase) #'e))]
         [(case-lambda (fmls e^ ... e) ...)
          `(case-lambda ,stx
                        [,(syntax->list #'(fmls ...))
                         ,(stx-map (λ (b) (stx-map (expr phase) b))
                                   #'((e^ ...) ...))
                         ...
                         ,(stx-map (expr phase) #'(e ...))]
                        ...)]
         [(if e0 e1 e2)
          `(if ,stx ,((expr phase) #'e0)
               ,((expr phase) #'e1)
               ,((expr phase) #'e2))]
         [(begin e^ ... e)
          `(begin ,stx ,(stx-map (expr phase) #'(e^ ...)) ...
                  ,((expr phase) #'e))]
         [(begin0 e e^ ...)
          `(begin0 ,stx ,((expr phase) #'e)
                   ,(stx-map (expr phase) #'(e^ ...)) ...)]
         [(let-values ([(id ...) e] ...)
            b^ ... b)
          `(let-values ,stx ([(,(stx-map syntax->list #'((id ...) ...)) ...)
                              ,(stx-map (expr phase) #'(e ...))] ...)
             ,(stx-map (expr phase) #'(b^ ...)) ...
             ,((expr phase) #'b))]
         [(letrec-values ([(id ...) e] ...)
            b^ ... b)
          `(letrec-values ,stx ([(,(stx-map syntax->list #'((id ...) ...)) ...)
                                 ,(stx-map (expr phase) #'(e ...))] ...)
             ,(stx-map (expr phase) #'(b^ ...)) ...
             ,((expr phase) #'b))]
         [(set! id e)
          `(set! ,stx ,#'id ,((expr phase) #'e))]
         [(quote d)
          `(quote ,stx ,#'d)]
         [(quote-syntax d)
          `(quote-syntax ,stx ,#'d)]
         [(quote-syntax d #:local)
          `(quote-syntax-local ,stx ,#'d)]
         [(with-continuation-mark e0 e1 e2)
          `(with-continuation-mark ,stx ,((expr phase) #'e0)
             ,((expr phase) #'e1) ,((expr phase) #'e2))]
         [(#%plain-app e^ e ...)
          `(#%plain-app ,stx ,((expr phase) #'e^)
                        ,(stx-map (expr phase) #'(e ...)) ...)]
         [(#%top . id)
          `(#%top ,stx ,#'id)]
         [(#%variable-reference id)
          `(#%variable-reference ,stx ,#'id)]
         [(#%variable-reference (#%top . id))
          `(#%variable-reference-top ,stx ,#'id)]
         [(#%variable-reference)
          `(#%variable-reference-null ,stx)]))))
  (case start
    [(top-level-form)
     ((top-level-form phase) stx)]
    [(module-level-form)
     ((module-level-form phase) stx)]
    [(expr)
     ((expr phase) stx)]))