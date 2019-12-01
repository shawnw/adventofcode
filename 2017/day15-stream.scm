(import (srfi 41))

(define (rng factor seed)
  (let ((next (lambda (state)
                (remainder (* state factor) 2147483647))))
    (stream-unfold (lambda (x) x)
                   (lambda (x) #t)
                   next
                   (next seed))))

(define seeda 883)
(define seedb 879)
                  
(define a (rng 16807 seeda))
(define b (rng 48271 seedb))
(define a2 (stream-filter (lambda (n) (= (remainder n 4) 0)) a))
(define b2 (stream-filter (lambda (n) (= (remainder n 8) 0)) b))

(define (judge count seqa seqb)
  (let ((seqa (stream-take count seqa))
        (seqb (stream-take count seqb)))        
    (let loop ((score 0)
               (vala (stream-car seqa))
               (valb (stream-car seqb))
               (seqa (stream-cdr seqa))
               (seqb (stream-cdr seqb)))
    (if (stream-null? vala)
        score
        (loop (if (= (bitwise-and vala #xFFFF)
                     (bitwise-and valb #xFFFF))
                  (+ score 1)
                  score)
              (stream-car seqa)
              (stream-car seqb)
              (stream-cdr seqa)
              (stream-cdr seqb))))))
    
(format #t "Part 1: ~A~%" (judge 40000000 a b))
(format #t "Part 2: ~A~%" (judge  5000000 a2 b2))
