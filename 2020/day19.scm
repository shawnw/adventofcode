#!/usr/local/bin/csi -s
(import (chicken format)
        (chicken io)
        (chicken string)
        (srfi 1)
        (srfi 13)
        (srfi 14)
        )
(declare (fixnum-arithmetic) (block))

(define (read-rules #!optional (port (current-input-port)))
  (let loop ((acc '())
             (line (read-line port)))
    (if (or (eof-object? line) (string=? line ""))
        (reverse! acc)
        (loop (cons line acc) (read-line port)))))

(define (max-ruleno rules)
  (+ 1 (fold (lambda (r acc) (max (car r) acc)) 0 rules)))

(define (numbers->list nums)
  (map string->number (string-split nums " ")))

(define (parse-branches branches)
  (map (lambda (branch)
         (let ((branch (string-trim-both branch char-set:whitespace)))
           (if (char=? (string-ref branch 0) #\")
               (string-ref branch 1)
               (numbers->list branch)))) branches))

(define (parse-rules raw)
  (let loop ((rules raw)
              (acc '()))
    (if (null? rules)
        (let* ((size (max-ruleno acc))
               (rulevec (make-vector size #f)))
          (for-each (lambda (rule)
                      (vector-set! rulevec (car rule) (cdr rule)))
                    acc)
          rulevec)
        (let* ((parts (string-split (car rules) ":"))
               (ruleno (string->number (car parts)))
               (branches (string-split (second parts) "|")))
          (loop (cdr rules) (cons (cons ruleno (parse-branches branches)) acc))))))

(define (match-rules line rules)
  (letrec ((match-helper
            (lambda (ridx lidx)
              (any (lambda (ruleset)
                     (let loop ((ruleset ruleset)
                                (lidx lidx))
                       (cond ((null? ruleset) lidx)
                             ((char? ruleset)
                              (if (char=? (string-ref line lidx) ruleset)
                                  (+ lidx 1)
                                  #f))
                             (else
                              (let ((res (match-helper (car ruleset) lidx)))
                                (if (number? res)
                                    (loop (cdr ruleset) res)
                                    res))))))
                   (vector-ref rules ridx)))))
    (let ((match (match-helper 0 0)))
      (if (number? match)
          (= (string-length line) match)
          match))))

(define (solve1 lines rules)
  (count (cut match-rules <> rules) lines))

(define rules (parse-rules (read-rules)))
(define lines (read-lines))
(printf "Part 1: ~A~%" (solve1 lines rules))
