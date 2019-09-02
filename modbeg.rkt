#lang racket/base
(require (prefix-in p: (only-in racket/base #%module-begin))
         (for-syntax racket/base syntax/stx "fully-expanded-program.rkt"))

(define-syntax-rule (define/provide-module-begin pass)
  (begin
    (define-syntax (my-module-begin stx)
      (syntax-case stx ()
        [(_ form (... ...))
         (begin
           (define expanded
             (local-expand #'(p:#%module-begin form (... ...)) 'module-begin '()))
           (define prog
             (syntax-case expanded ()
               [(_ form (... ...))
                (stx-map (λ (s) (syntax->FullyExpandedProgram s 'module-level-form))
                         #'(form (... ...)))]))
           (define transed (map pass prog))
           (define ret
             #`(#%plain-module-begin
                #,@(map (λ (s) (FullyExpandedProgram->syntax s 'module-level-form -1))
                        transed)))
           ret)]))
    (provide (rename-out [my-module-begin #%module-begin]))))

(provide define/provide-module-begin)