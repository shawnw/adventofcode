;;; kawa day01-2.scm < day01.txt
(cond-expand
  (kawa
   (import (srfi 1)
           (srfi 158)
           (srfi 127)))
  (chicken
   (require-extension srfi-121)
   (require-extension srfi-127)))
(define (transform str)
  (map (lambda (c) (- (char->integer c) (char->integer #\0)))
       (string->list str)))
(define (add-if= a b sum) (if (= a b) (+ a sum) sum))
(define (captcha lst)
  (let* ((g1 (gdrop (apply circular-generator lst) 1))
         (g2 (gdrop (apply circular-generator lst) (div (length lst) 2)))
         (res
          (fold (lambda (n acc)
                  (list
                   (lseq-cdr (first acc))
                   (add-if= n (lseq-car (first acc)) (second acc))
                   (lseq-cdr (third acc))
                   (add-if= n (lseq-car (third acc)) (fourth acc))))
                (list (generator->lseq g1)
                      0
                      (generator->lseq g2)
                      0)
                lst)))
    (values (second res) (fourth res))))
               
(display "Tests (part 1):")
(newline)
(for-each (lambda (input)
            (let-values (((part1 part2) (captcha (transform input))))
              (format #t "~A: ~A~%" input part1)))
          '("1122" "1111" "1234" "91212129"))
(display "Tests (part 2):\n")
(for-each (lambda (input)
            (let-values (((part1 part2) (captcha (transform input))))
              (format #t "~A: ~A~%" input part2)))
          '("1212" "1221" "123425" "123123" "12131415"))

(let-values (((part1 part2) (captcha (transform (read-line)))))
  (format #t "Part 1: ~A~%Part 2: ~A~%" part1 part2))
