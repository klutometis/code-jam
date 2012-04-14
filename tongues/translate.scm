
(include "translation.scm")

(define (translate input)
  (string-map (lambda (transferendum)
                (hash-table-ref translation transferendum))
              input))
