#lang racket/base
(require (prefix-in p: (only-in racket/base #%module-begin))
         syntax/parse/define
         (for-syntax racket/base nanopass/base
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
             (syntax->FullyExpandedProgram expanded 'module-begin-form))
           (define transed (pass prog))
           (define ret
             (FullyExpandedProgram->syntax transed 'module-begin-form))
           ret)]))
    (provide (rename-out [my-module-begin #%module-begin]))))

(begin-for-syntax
  (define ((module-begin pass) s)
    (nanopass-case
     (FullyExpandedProgram ModuleBeginForm) s
     [(#%plain-module-begin ,s ,ml ...)
      (with-output-language (FullyExpandedProgram ModuleBeginForm)
        `(#%plain-module-begin
          ,s
          ,(for/list ([ml (in-list ml)])
             (pass ml))
          ...))])))

(define-simple-macro (define/provide-module-begin pass:id)
  (begin
    (define-for-syntax f (module-begin pass))
    (define/provide-module-begin* f)))

(provide define/provide-module-begin* define/provide-module-begin)