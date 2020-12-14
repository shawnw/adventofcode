#!/usr/local/bin/csi -s
(import (chicken format)
        (chicken io)
        (chicken irregex)
        (srfi 1)
        (srfi 13)
        (srfi 69))
(declare (fixnum-arithmetic) (block))

(define mask-re (sre->irregex '(: "mask = " ($ (+ (or #\1 #\0 #\X))))))
(define mem-re (sre->irregex '(: "mem[" (=> address (+ digit)) "] = "
                                 (=> value (+ digit)))))

(define (read-input #!optional (port (current-input-port)))
  (let loop ((line (read-line port))
             (acc '()))
    (if (eof-object? line)
        (reverse! acc)
        (let ((mask-match (irregex-match mask-re line))
              (mem-match (irregex-match mem-re line)))
          (cond
           (mask-match
            (loop (read-line port)
                  (cons (cons 'mask (irregex-match-substring mask-match 1))
                        acc)))
           (mem-match
            (loop (read-line port)
                  (cons (cons 'mem
                              (cons (string-pad
                                     (number->string
                                      (string->number
                                       (irregex-match-substring mem-match 'address))
                                      2)
                                     36 #\0)
                                    (string-pad
                                     (number->string
                                      (string->number
                                       (irregex-match-substring mem-match 'value))
                                      2)
                                     36 #\0)))
                        acc)))
           (else (error "Unknown line " line)))))))

(define (apply-mask1 mask num)
  (let ((num (string-copy num)))
    (string-for-each-index
     (lambda (i)
       (case (string-ref mask i)
         ((#\1 #\0) => (cut string-set! num i <>)))) mask)
    num))

(define (sum-memory mem)
  (hash-table-fold mem (lambda (_ val total) (+ total (string->number val 2))) 0))

(define (set-memory! mem addr val mask)
  (let ((addr (string-copy addr))
        (floaters '()))
    (string-for-each-index
     (lambda (i)
       (case (string-ref mask i)
         ((#\1)
          (string-set! addr i #\1))
         ((#\X)
          (set! floaters (cons i floaters)))))
     mask)
    (let loop ((floaters floaters))
      (if (null? floaters)
          (hash-table-set! mem (string-copy addr) val)
          (let ((i (car floaters)))
            (string-set! addr i #\1)
            (loop (cdr floaters))
            (string-set! addr i #\0)
            (loop (cdr floaters)))))))

(define (solve1 input)
  (let ((mem (make-hash-table string=? string-hash)))
    (let loop ((input input)
               (mask '()))
      (cond
       ((null? input)
        (sum-memory mem))
       ((eq? (caar input) 'mask)
        (loop (cdr input) (cdar input)))
       ((eq? (caar input) 'mem)
        (hash-table-set! mem (string-copy (cadar input))
                         (apply-mask1 mask (cddar input)))
        (loop (cdr input) mask))))))

(define (solve2 input)
  (let ((mem (make-hash-table string=? string-hash)))
    (let loop ((input input)
               (mask '()))
      (cond
       ((null? input)
        (sum-memory mem))
       ((eq? (caar input) 'mask)
        (loop (cdr input) (cdar input)))
       ((eq? (caar input) 'mem)
        (set-memory! mem (cadar input) (cddar input) mask)
        (loop (cdr input) mask))))))

(define input (read-input))
(printf "Part 1: ~A~%" (solve1 input))
(printf "Part 2: ~A~%" (solve2 input))
