
(require 'srfi-69 'srfi-13)

(use test srfi-95)

;;; Initial hypothesis
(define translation
  (alist->hash-table
   '(;; From the problem specification
     (#\a . #\y)
     (#\o . #\e)
     (#\z . #\q)
     ;; By process of elimination from below (a circular dependency).
     (#\q . #\z))))

(define inputs->outputs
  '(("ejp mysljylc kd kxveddknmc re jsicpdrysi"
     "our language is impossible to understand")
    ("rbcpc ypc rtcsra dkh wyfrepkym veddknkmkrkcd"
     "there are twenty six factorial possibilities")
    ("de kr kd eoya kw aej tysr re ujdr lkgc jv"
     "so it is okay if you want to just give up")))

;;; Modify the translation based on the input->output mapping.
(for-each (lambda (input->output)
            (receive (input output)
              (apply values input->output)
              (for-each (lambda (from to)
                          (hash-table-set! translation from to))
                        (string->list input)
                        (string->list output))))
          inputs->outputs)
