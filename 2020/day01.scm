#!/usr/local/bin/csi -s
(import (chicken format)
        (chicken io)
        (srfi 1)
        (srfi 158))

;;; Generate all combinations of length len
(define (generate-combinations len data)
  (make-coroutine-generator
   (lambda (yield)
     (if (= 1 len)
         (for-each (lambda (item) (yield (list item))) data)
         (let loop ((data data))
           (if (not (null? data))
               (let ((head (car data)))
                 (generator-for-each
                  yield
                  (gmap (cut cons head <>)
                        (generate-combinations (- len 1) (cdr data))))
                 (loop (cdr data)))))))))

(define (find-totals numbers goal len)
  (generator-find (lambda (sublist) (= goal (apply + sublist)))
        (generate-combinations len numbers)))

(define (solve input len)
  (and-let* ((nums (find-totals input 2020 len)))
            (apply * nums)))

(define input (read-list))
(printf "Part 1: ~A~%" (solve input 2))
(printf "Part 2: ~A~%" (solve input 3))
