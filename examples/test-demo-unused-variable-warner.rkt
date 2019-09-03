#lang s-exp "demo-unused-variable-warner.rkt"
(require (for-syntax racket/base))
(begin-for-syntax (module a racket
                    (provide x)
                    (define x 1)))

