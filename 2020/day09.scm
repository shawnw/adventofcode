#!/usr/local/bin/csi -s
(import (chicken format)
        (chicken io)
        (srfi 1)
        (srfi 158)
        (srfi 193))
(declare (fixnum-arithmetic) (block))

(define (sums-of-pairs numbers)
  (make-coroutine-generator
   (lambda (yield)
     (pair-for-each
      (lambda (pair)
        (let ((num1 (car pair)))
          (for-each (lambda (num2) (yield (+ num1 num2))) (cdr pair))))
      numbers))))

(define (solve1 numbers window-length)
  (let-values (((preamble numbers) (split-at numbers window-length))
               ((wl-1) (sub1 window-length)))
    (let loop ((numbers numbers)
               (preamble (reverse! preamble)))
      (if (null? numbers)
          #f
          (let ((candidate (car numbers)))
            (if (generator-find (cut = <> candidate) (sums-of-pairs preamble))
                (loop (cdr numbers) (cons candidate (take! preamble wl-1)))
                candidate))))))

(define (numbers-to-total numbers goal)
  (let loop ((numbers numbers)
             (total 0)
             (acc '()))
    (cond
     ((= total goal) (reverse! acc))
     ((or (> total goal) (null? numbers)) #f)
     (else
      (loop (cdr numbers) (+ (car numbers) total) (cons (car numbers) acc))))))

(define (minmax lst #!optional (less-than? <))
  (apply values
         (fold (lambda (elem minmax)
                 (cons (if (less-than? elem (car minmax)) elem (car minmax))
                       (if (less-than? (cdr minmax) elem) elem (cdr minmax))))
               (cons (car lst) (car lst)) (cdr lst))))

(define (solve2 numbers key)
  (let loop ((numbers numbers))
    (cond
     ((null? numbers) #f)
     ((numbers-to-total numbers key) =>
      (lambda (range) (call-with-values (lambda () (minmax range)) +)))
     (else (loop (cdr numbers))))))

;;; Take an optional window size from the first command line argument
(define window-size
  (let ((cl (command-line)))
    (if (>= (length cl) 2)
        (string->number (cadr cl))
        25)))
(define input (read-list))
(define key (solve1 input window-size))
(printf "Part 1: ~A~%" key)
(printf "Part 2: ~A~%" (solve2 input key))
