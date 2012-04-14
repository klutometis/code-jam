#!/usr/bin/env chicken-scheme

(require 'srfi-13 'srfi-1)

(use debug extras)

(define (best triplet)
  (apply max triplet))

(define (term total)
  (inexact->exact (floor (/ total 3))))

(define (surprising-triplet total)
  (let ((term (term total)))
    (cond ((= (modulo total 3) 2)
           (list term term (+ term 2)))
          ((= (modulo total 3) 1)
           (list (+ term 1) (+ term 1) (- term 1)))
          (else (list (+ term 1) term (- term 1))))))

(define (unsurprising-triplet total)
  (let ((term (term total)))
    (cond ((= (modulo total 3) 2)
           (list (+ term 1) (+ term 1) term))
          ((= (modulo total 3) 1)
           (list (+ term 1) term term))
          (else (list term term term)))))

(define (best-greater-than triplets threshold)
  (count (lambda (best) (>= best threshold))
         (map best triplets)))

(define (balance-triplets totals surprises threshold)
  (let iter ((totals (sort totals >))
             (surprises surprises)
             (triplets '()))
    (if (null? totals)
        (best-greater-than triplets threshold)
        (let ((total (car totals)))
          ;; Don't fuck around with surprises if we don't have any
          ;; left.
          (if (zero? surprises)
              (iter (cdr totals)
                    surprises
                    (cons (unsurprising-triplet total) triplets))
              (let ((surprising-triplet (surprising-triplet total))
                    (unsurprising-triplet (unsurprising-triplet total)))
                ;; Actually use a surprise if it makes a difference
                ;; (i.e. propels us above the threshold).
                (if (and (>= (best surprising-triplet) threshold)
                         (< (best unsurprising-triplet) threshold)
                         (every positive? surprising-triplet))
                 (iter (cdr totals)
                       (- surprises 1)
                       (cons surprising-triplet triplets))
                 (iter (cdr totals)
                       surprises
                       (cons unsurprising-triplet triplets)))))))))

(define (display-case best-greater-than number)
  (format #t "Case #~a: ~a~%" number best-greater-than))

(define (process-input)
  (let ((T (read-line)))
    (let read ((line (read-line))
               (case 1))
      (if (not (eof-object? line))
          (let ((line (map string->number (string-tokenize line))))
            (let ((N (car line))
                  (surprises (cadr line))
                  (threshold (caddr line))
                  (totals (cdddr line)))
              (display-case
               (balance-triplets totals surprises threshold)
               case)
              (read (read-line) (+ case 1))))))))

(process-input)
