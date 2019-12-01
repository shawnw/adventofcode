(cond-expand
 (kawa
  (import (srfi 158) (rnrs arithmetic fixnums)))
 (chicken
  (require-extension srfi-121)))

(define (make-rng factor seed)
  (let ((next (lambda (state)
                (remainder (* state factor) 2147483647)))
        (state seed))
    (lambda ()
      (set! state (next state))
      state)))

(define factora 16807)
(define factorb 48271)
(define seeda 883)
(define seedb 879)
                  
(define a (make-rng factora seeda))
(define b (make-rng factorb seedb))
(define a2 (gfilter (lambda (n) (= (fxmod n 4) 0))
                        (make-rng factora seeda)))
(define b2 (gfilter (lambda (n) (= (fxmod n 8) 0))
                        (make-rng factorb seedb)))

(define (judge count seqa seqb)
  (let ((seqeq
         (gcombine (lambda (a b x) ;; srfi 121 doesn't have 158's gmap, sigh
                     (values (= (fxand a #xFFFF)
                                  (fxand b #xFFFF)) x))
                   #f seqa seqb)))
    (generator-count (lambda (p) p) (gtake seqeq count))))

(format #t "Test 1: ~A~%" (judge 5
                                 (make-rng factora 65)
                                 (make-rng factorb 8921)))
(format #t "Part 1: ~A~%" (judge 40000000 a b))
(format #t "Part 2: ~A~%" (judge  5000000 a2 b2))
