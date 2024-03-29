#!/usr/local/bin/csi -s
(import (chicken format)
        (chicken io)
        (srfi 1)
        (srfi 13)
        (srfi 14))
(declare (fixnum-arithmetic) (block))

(define (skip-whitespace s)
  (let ((idx (string-skip s char-set:whitespace)))
    (if idx
        (substring/shared s idx)
        s)))

(define (next-token s)
  (let ((s (skip-whitespace s)))
    (if (string=? s "")
        (values 'empty #f "")
        (case (string-ref s 0)
          ((#\() (values 'lparen #\( (substring/shared s 1)))
          ((#\)) (values 'rparen #\) (substring/shared s 1)))
          ((#\+ #\*) (values 'operator (string-ref s 0)
                             (substring/shared s 1)))
          ((#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9)
           (let ((non-digit (string-skip s char-set:digit)))
             (if non-digit
                 (values 'number (string->number (substring/shared s 0 non-digit))
                         (substring/shared s non-digit))
                 (values 'number (string->number s) ""))))
          (else (values 'invalid (string-ref s 0) (substring/shared s 1)))))))

;;; Basic shunting-yard algorithm
(define (parse-expr s #!optional use-precedence)
  (let loop ((s s)
             (output-queue '())
             (operator-stack '()))
    (let-values (((tok val rest) (next-token s)))
      (case tok
        ((number)
         (loop rest (cons val output-queue) operator-stack))
        ((operator)
         (cond
          ((null? operator-stack)
           (loop rest output-queue (cons val operator-stack)))
          (use-precedence
           (do ()
               ((or (null? operator-stack)
                    (char=? (car operator-stack) #\()
                    (and (char=? (car operator-stack) #\*)
                         (char=? val #\+)))
                (loop rest output-queue (cons val operator-stack)))
             (set! output-queue (cons (car operator-stack) output-queue))
             (set! operator-stack (cdr operator-stack))))
          (else
           (do ()
               ((or (null? operator-stack)
                    (char=? (car operator-stack) #\())
                (loop rest output-queue (cons val operator-stack)))
             (set! output-queue (cons (car operator-stack) output-queue))
             (set! operator-stack (cdr operator-stack))))))
        ((lparen)
         (loop rest output-queue (cons val operator-stack)))
        ((rparen)
         (do ()
             ((char=? (car operator-stack) #\()
              (loop rest output-queue (cdr operator-stack)))
           (set! output-queue (cons (car operator-stack) output-queue))
           (set! operator-stack (cdr operator-stack))))
        ((empty)
         (if (null? operator-stack)
             (reverse! output-queue)
             (do ()
                 ((null? operator-stack) (reverse! output-queue))
               (if (char=? (car operator-stack) #\()
                   (error "Unbalanced parenthesis")
                   (begin
                     (set! output-queue (cons (car operator-stack) output-queue))
                     (set! operator-stack (cdr operator-stack)))))))
        ((invalid) (error "Invalid token " tok))))))

(define (eval-expr expr)
  (let loop ((expr expr)
             (stack '()))
    (if (null? expr)
        (if (= (length stack) 1)
            (car stack)
            (error "Unbalanced expression stack"))
        (let ((top (car expr)))
          (if (number? top)
              (loop (cdr expr) (cons top stack))
              (let ((num1 (first stack))
                    (num2 (second stack)))
                (loop (cdr expr)
                      (cons (case top
                              ((#\+) (+ num1 num2))
                              ((#\*) (* num1 num2)))
                            (cddr stack)))))))))

(define (parse-and-eval s) (eval-expr (parse-expr s)))

(define test1-cases '(("1 + 2 * 3 + 4 * 5 + 6" . 71)
                     ("2 * 3 + (4 * 5)" . 26)
                     ("5 + (8 * 3 + 9 + 3 * 4 * 3)" . 437)
                     ("5 * 9 * (7 * 3 * 3 + 9 * 3 + (8 + 6 * 4))" . 12240)
                     ("((2 + 4 * 9) * (6 + 9 * 8 + 6) + 6) + 2 + 4 * 2" . 13632)))
(define (test1 test-case)
  (let* ((expr (parse-expr (car test-case)))
         (res (eval-expr expr)))
    (printf "~A => ~S => ~A ~A~%" (car test-case) expr res
            (if (= res (cdr test-case)) "PASS" "FAIL"))))
(for-each test1 test1-cases)

(define test2-cases '(("1 + 2 * 3 + 4 * 5 + 6" . 231)
                      ("1 + (2 * 3) + (4 * (5 + 6))" . 51)
                      ("2 * 3 + (4 * 5)" . 46)
                      ("5 + (8 * 3 + 9 + 3 * 4 * 3)" . 1445)
                      ("5 * 9 * (7 * 3 * 3 + 9 * 3 + (8 + 6 * 4))" . 669060)
                      ("((2 + 4 * 9) * (6 + 9 * 8 + 6) + 6) + 2 + 4 * 2" . 23340)))
(define (test2 test-case)
  (let* ((expr (parse-expr (car test-case) #t))
         (res (eval-expr expr)))
    (printf "~A => ~S => ~A ~A~%" (car test-case) expr res
            (if (= res (cdr test-case)) "PASS" "FAIL"))))
(for-each test2 test2-cases)

(define input (read-lines))
(define (solve1 input)
  (reduce + 0 (map (lambda (s) (eval-expr (parse-expr s))) input)))
(define (solve2 input)
  (reduce + 0 (map (lambda (s) (eval-expr (parse-expr s #t))) input)))


(printf "Part 1: ~A~%" (solve1 input))
(printf "Part 2: ~A~%" (solve2 input))
