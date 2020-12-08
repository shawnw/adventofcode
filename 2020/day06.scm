#!/usr/local/bin/csi -s
(import
 (chicken format)
 (chicken io)
 (srfi 1)
 (srfi 14))
(declare (fixnum-arithmetic) (block))

(define (read-group #!optional (port (current-input-port)))
  (let loop ((answers '())
             (line (read-line port)))
    (if (or (eof-object? line) (string=? line ""))
        answers
        (loop (cons (string->char-set line) answers) (read-line port)))))

(define (read-groups #!optional (port (current-input-port)))
  (let loop ((groups '()))
    (let ((new-group (read-group port)))
      (if (null? new-group)
          groups
          (loop (cons new-group groups))))))

(define (solve reduce-op groups)
  (fold (lambda (group total)
          (+ total (char-set-size (reduce reduce-op (char-set) group))))
        0 groups))

(define (solve1 groups) (solve char-set-union groups))
(define (solve2 groups) (solve char-set-intersection groups))

(define input (read-groups))
(printf "Part 1: ~A~%" (solve1 input))
(printf "Part 2: ~A~%" (solve2 input))
