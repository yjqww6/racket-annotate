#lang s-exp "demo.rkt"

(for ([i (in-range 10)])
  (void))

(module+ main
  (for ([i (in-range 10)])
    (void)))