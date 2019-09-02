#lang s-exp "demo-unused-variable-warner.rkt"
(let ([x 1])
  (let ([y 1])
    (+ x 1)))