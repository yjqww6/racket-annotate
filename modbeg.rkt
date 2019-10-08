#lang racket/base
(require (for-syntax racket/base) syntax/parse/define)

(module mod racket/base
  (require (for-template (prefix-in p: (only-in racket/base #%module-begin)))
           nanopass/base
           syntax/stx "fully-expanded-program.rkt")
  (provide module-begin module-begin*)
  
  (define code-insp
    (variable-reference->module-declaration-inspector
     (#%variable-reference)))
  
  (define ((module-begin* pass) stx)
    (syntax-case stx ()
      [(_ form ...)
       (begin
         (define expanded
           (local-expand #'(p:#%module-begin form ...) 'module-begin '()))
         (parameterize ([current-code-inspector code-insp])
           (define prog
             (syntax->FullyExpandedProgram expanded 'module-begin-form))
           (define transed (pass prog))
           (define ret
             (FullyExpandedProgram->syntax transed 'module-begin-form))
           ret))]))
  
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

(require (for-syntax 'mod))

(define-simple-macro (define/provide-module-begin* pass:id)
  (begin
    (define-syntax my-module-begin (module-begin* pass))
    (provide (rename-out [my-module-begin #%module-begin]))))


(define-simple-macro (define/provide-module-begin pass:id)
  (begin
    (define-for-syntax f (module-begin pass))
    (define/provide-module-begin* f)))

(provide define/provide-module-begin* define/provide-module-begin)