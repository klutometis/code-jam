#!/usr/bin/env chicken-scheme

(include "translate.scm")

(use extras)

(define (display-case transferendum number)
  (format #t "Case #~a: ~a~%" number (translate transferendum)))

(define (translate-input)
  (let ((test-cases (read-line)))
    (let read ((transferendum (read-line))
               (case 1))
      (if (not (eof-object? transferendum))
          (begin
            (display-case transferendum case)
            (read (read-line) (+ case 1)))))))

(translate-input)
