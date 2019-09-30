#lang racket/base

(require (for-syntax racket/base syntax/parse syntax/parse/lib/function-header)
         racket/stxparam racket/performance-hint)

(provide (all-defined-out))

(begin-for-syntax
  (define (id->key id)
    (datum->syntax #f (string->keyword (symbol->string (syntax-e id))))))

(define-syntax define-implicit-key
  (syntax-parser
    [(_ name:id)
     #'(define-syntax-parameter name
         (λ (stx)
           (raise-syntax-error #f (format "unbound implicit parameter: ~a" 'name)
                               stx)))]))

(define-syntax define-implicit
  (syntax-parser
    [(_ (name:id . args:formals) (key:id ...) body:expr ...+)
     #:with (k ...) (map id->key (syntax->list #'(key ...)))
     #:with (tmp-k ...) (generate-temporaries #'(key ...))
     #:with (explicit wrapper) (generate-temporaries #'(name name))
     #:with (params ...) #'args.params
     (syntax-property
      #'(begin
          (define (explicit tmp-k ... params ...)
            (syntax-parameterize ([key (make-rename-transformer #'tmp-k)]
                                  ...)
              body ...))
          (define-inline (wrapper tmp-k ... . args)
            (explicit tmp-k ... params ...))
          (define-syntax name
            (syntax-parser
              [(_ (~alt (~optional (~seq k tmp-k)
                                   #:defaults ([tmp-k #'key]))
                        ...
                        other)
                  (... ...))
               #'(wrapper tmp-k ... other (... ...))]
              [_:id #'(λ ((~@ k [tmp-k key]) ... . args)
                        (explicit tmp-k ... params ...))])))
      'disappeared-use
      (map syntax-local-introduce (syntax->list #'(key ...))))]))

(define-syntax with-implicit
  (syntax-parser
    [(_ ([k:id val] ...) body:expr ...+)
     #:with (tmp ...) (generate-temporaries #'(val ...))
     (syntax-property
      #'(let ([tmp val] ...)
          (syntax-parameterize ([k (make-rename-transformer #'tmp)] ...)
            body ...))
      'disappeared-use
      (map syntax-local-introduce (syntax->list #'(k ...))))]))
