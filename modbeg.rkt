#lang racket/base
(require (prefix-in p: (only-in racket/base #%module-begin))
         syntax/parse/define
         (for-syntax racket/base
                     syntax/stx "fully-expanded-program.rkt"))

(define-simple-macro (define/provide-module-begin* pass:id)
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
           (define transed (pass prog))
           (define ret
             #`(#%plain-module-begin
                #,@(map (λ (s) (FullyExpandedProgram->syntax s 'module-level-form))
                        transed)))
           ret)]))
    (provide (rename-out [my-module-begin #%module-begin]))))

(define-simple-macro (define/provide-module-begin pass:id)
  (begin
    (define-for-syntax (f s)
      (for/list ([s (in-list s)])
        (pass s)))
    (define/provide-module-begin* f)))

(provide define/provide-module-begin* define/provide-module-begin)