#lang racket/base
(require (for-syntax "fpe.rkt" racket/base nanopass/base syntax/stx syntax/name)
         (prefix-in p: (only-in racket/base #%module-begin)))

(provide (all-from-out racket/base) #%module-begin)

(begin-for-syntax
  (define (build-trace name fmls)
    (define a
      (syntax-case fmls ()
        [(a ... . b)
         (identifier? #'b)
         #`(#%plain-app list* '#,name (~@ 'a a) ... 'b b)]
        [(a ...)
         #`(#%plain-app list '#,name (~@ 'a a) ...)]))
    #`(#%plain-app
       displayln
       #,a))
  (define-pass trace : (FullyExpandedProgram ModuleLevelForm) (fpe) ->
    (FullyExpandedProgram ModuleLevelForm) ()
    (Expr
     : Expr (fpe) -> Expr ()
     [(#%plain-lambda ,s ,fml ,[body*] ... ,[body])
      (cond
        [(syntax-local-infer-name s #f)
         =>
         (λ (n)
           (define name (datum->syntax #f n))
           (define tr
             (syntax->FullyExpandedProgram
              (build-trace name fml)
              'expr))
           `(#%plain-lambda ,s ,fml ,(cons tr body*) ... ,body))]
        [else
         `(#%plain-lambda ,s ,fml ,body* ... ,body)])]
     [(case-lambda ,s [,fml ,[body*] ... ,[body]] ...)
      (cond
        [(syntax-local-infer-name s #f)
         =>
         (λ (n)
           (define name (datum->syntax #f n))
           (let ([body*
                  (for/list ([fml (in-list fml)]
                             [body* (in-list body*)])
                    (define tr
                      (syntax->FullyExpandedProgram
                       (build-trace name fml)
                       'expr))
                    (cons tr body*))])
             `(case-lambda ,s [,fml ,body* ... ,body] ...)))]
        [else
         `(case-lambda ,s [,fml ,body* ... ,body] ...)])])
    (GeneralTopLevelForm
     : GeneralTopLevelForm (fpe) -> GeneralTopLevelForm ()
     [(define-syntaxes ,s (,x* ...) ,e)
      `(define-syntaxes ,s (,x* ...) ,e)])
    (SubModuleForm : SubModuleForm (fpe) -> SubModuleForm ())
    (ModuleLevelForm
     : ModuleLevelForm (fpe) -> ModuleLevelForm ()
     [(begin-for-syntax ,s ,ml* ...)
      `(begin-for-syntax ,s ,ml* ...)])
    (TopLevelForm
     : TopLevelForm (fpe) -> TopLevelForm ()
     [(tl:begin-for-syntax ,s ,tl* ...)
      `(tl:begin-for-syntax ,s ,tl* ...)])

    (ModuleLevelForm fpe))
  )

(define-syntax (#%module-begin stx)
  (syntax-case stx ()
    [(_ form ...)
     (begin
       (define expanded
         (local-expand #'(p:#%module-begin form ...) 'module-begin '()))
       (define fpe
         (syntax-case expanded ()
           [(_ form ...)
            (stx-map (λ (s) (syntax->FullyExpandedProgram s 'module-level-form))
                     #'(form ...))]))
       (define transed (map trace fpe))
       (define ret
         #`(#%plain-module-begin
            #,@(map (λ (s) (FullyExpandedProgram->syntax s 'module-level-form -1))
                    transed)))
       ret)]))