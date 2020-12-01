#!/usr/local/bin/csi -s
(require-extension (srfi 1))
(require-extension (chicken format))

; Generate all combinations of length len
(define (generate-combinations len data)
  (cond
   ((null? data) '())
   ((= 1 len) (map list data))
   (else
    (let loop ((acc '())
                (data data))
       (if (null? data)
           acc
           (let ((head (car data)))
             (loop (append!
                    (map (cut cons head <>) (generate-combinations (- len 1) (cdr data)))
                    acc)
                   (cdr data))))))))

(define (find-totals numbers goal len)
  (find (lambda (sublist) (= goal (apply + sublist)))
        (generate-combinations len numbers)))

(define (solve input len)
  (let ((nums (find-totals input 2020 len)))
    (if nums
        (printf "~A~%" (apply * nums))
        (display "No solution found!\n"))))

(define input (let loop ((numbers '()))
                (let ((num (read)))
                  (if (eof-object? num)
                      numbers
                      (loop (cons num numbers))))))
(solve input 2)
(solve input 3)
