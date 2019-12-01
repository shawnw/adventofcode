(import (srfi 127) (srfi 158))

(define (rng factor::long seed::long)
  (let ((next (lambda (state::long)
                (remainder (* state factor) java.lang.Integer:MAX_VALUE)))
        (state seed))
    (lambda ()
      (set! state (next state))
      state)))

(define (make-rng-lseq factor seed) (generator->lseq (rng factor seed)))

(define factora 16807)
(define factorb 48271)
(define seeda 883)
(define seedb 879)
                  
(define a (make-rng-lseq factora seeda))
(define b (make-rng-lseq factorb seedb))
(define a2 (lseq-filter (lambda (n::int) (= (remainder n 4) 0))
                        (make-rng-lseq factora seeda)))
(define b2 (lseq-filter (lambda (n::int) (= (remainder n 8) 0))
                        (make-rng-lseq factorb seedb)))

(define (judge count seqa seqb)
  (let ((seqab (lseq-take (lseq-zip seqa seqb) count)))
    (lseq-length (lseq-filter
                  (lambda (p)
                    (let ((a ::int (car p))
                          (b ::int (cadr p)))
                      (= (bitwise-and a #xFFFF)
                         (bitwise-and b #xFFFF)))) seqab))))

(format #t "Test 1: ~A~%" (judge 5
                                 (make-rng-lseq factora 65)
                                 (make-rng-lseq factorb 8921)))
(format #t "Part 1: ~A~%" (judge 40000000 a b))
(format #t "Part 2: ~A~%" (judge  5000000 a2 b2))
