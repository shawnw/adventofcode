;;; Chicken 4 scheme
;;; Usage:
;;; $ csc -O5 day05.scm
;;; $ ./day05

(declare
 (fixnum)
 (usual-integrations)
 (uses extras data-structures srfi-1 srfi-13 srfi-14))

(define (reacts? a b) (and (not (char=? a b)) (char-ci=? a b)))

(define (react-units polymer accum)
  (cond
   ((null? polymer) (reverse! accum))
   ((null? (cdr polymer)) (reverse! (cons (car polymer) accum)))
   ((reacts? (car polymer) (cadr polymer))
    (react-units (cddr polymer) accum))
   (else
    (react-units (cdr polymer) (cons (car polymer) accum)))))

(define (part1 input)
  (let loop ((poly-last '())
             (poly-this (string->list input)))
    (if (equal? poly-last poly-this)
        (length poly-this)
        (loop poly-this (react-units poly-this '())))))

(define (part2 input)
  (let ((chars (char-set->list (string->char-set (string-upcase input)))))
    (fold (lambda (ch minlen)
            (min minlen
                 (part1 (string-delete (lambda (c)
                                         (char-ci=? c ch)) input))))
          (string-length input) chars)))
    
(define sample "dabAcCaCBAcCcaDA")
(define input-text (string-chomp (with-input-from-file "day05.txt" read-string)))

(printf "Test 1: ~A~%" (part1 sample))
(printf "Part 1: ~A~%" (part1 input-text))

(printf "Test 2: ~A~%" (part2 sample))
(printf "Part 2: ~A~%" (part2 input-text))
