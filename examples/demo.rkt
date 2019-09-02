#lang racket/base
(require (for-syntax "../main.rkt" racket/base nanopass/base
                     syntax/name))

(provide (except-out (all-from-out racket/base) #%module-begin))

(begin-for-syntax
  
  (define (build-trace n fmls)
    (define name (datum->syntax #f n))
    (define a
      (syntax-case fmls ()
        [(a ... . b)
         (identifier? #'b)
         #`(#%plain-app list '#,name (~@ 'a a) ... 'b b)]
        [(a ...)
         #`(#%plain-app list '#,name (~@ 'a a) ...)]))
    (syntax->FullyExpandedProgram #`(#%plain-app displayln #,a) 'expr))
  
  (define-pass trace : (FullyExpandedProgram ModuleLevelForm) (prog) ->
    (FullyExpandedProgram ModuleLevelForm) ()
    (Expr
     : Expr (prog) -> Expr ()
     [(#%plain-lambda ,s ,fml ,[body*] ... ,[body])
      (cond
        [(syntax-local-infer-name s #f)
         =>
         (λ (name)
           (define tr (build-trace name fml))
           `(#%plain-lambda ,s ,fml ,(cons tr body*) ... ,body))]
        [else
         `(#%plain-lambda ,s ,fml ,body* ... ,body)])]
     [(case-lambda ,s [,fml ,[body*] ... ,[body]] ...)
      (cond
        [(syntax-local-infer-name s #f)
         =>
         (λ (name)
           (let ([body*
                  (for/list ([fml (in-list fml)]
                             [body* (in-list body*)])
                    (define tr (build-trace name fml))
                    (cons tr body*))])
             `(case-lambda ,s [,fml ,body* ... ,body] ...)))]
        [else
         `(case-lambda ,s [,fml ,body* ... ,body] ...)])])
    
    (GeneralTopLevelForm
     : GeneralTopLevelForm (prog) -> GeneralTopLevelForm ()
     [(define-syntaxes ,s (,x* ...) ,e)
      `(define-syntaxes ,s (,x* ...) ,e)])
    
    (SubModuleForm : SubModuleForm (prog) -> SubModuleForm ())
    
    (ModuleLevelForm
     : ModuleLevelForm (prog) -> ModuleLevelForm ()
     [(begin-for-syntax ,s ,ml* ...)
      `(begin-for-syntax ,s ,ml* ...)])

    (ModuleLevelForm prog))
  )

(define/provide-module-begin trace)