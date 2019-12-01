#!/usr/bin/csi -s

(use srfi-1)

(define (extract-paren lst depth accum)
  (cond
   ((null? lst) (values (reverse! accum) lst))
   ((char=? (car lst) #\))
    (if (= depth 1)
        (values (reverse! accum) (cdr lst))
        (extract-paren (cdr lst) (- depth 1) (cons #\) accum))))
   ((char=? (car lst) #\()
    (extract-paren (cdr lst) (+ depth 1) (cons #\( accum)))
   (else
    (extract-paren (cdr lst) depth (cons (car lst) accum)))))

(define (next-token lst)
  (case (car lst)
    ((#\N #\E #\S #\W #\|) (values (car lst) (cdr lst)))
    ((#\() (extract-paren (cdr lst) 1 '()))
    ((#\^ #\$ #\newline) (next-token (cdr lst)))
    (else (error "Unknown token" (car lst)))
    ))

(define (split-alternatives lst)
  (let loop ((lst2 lst) (accum '()))
    (let ((alt-idx (list-index (cut char=? <> #\|) lst2)))
      (if alt-idx
          (let-values (((before rest) (split-at lst2 (+ 1 alt-idx))))
            (loop rest (cons before accum)))
          (cons lst2 accum)))))

(define (find-length lst len)
  (if (null? lst)
      len
      (let-values (((token rest) (next-token lst)))
        (printf "token: ~S rest: ~S~%" token rest)
        (cond
         ((null? token) len)
         ((char? token)
          (find-length rest (+ len (if (char=? token #\|) 0 1))))
         ((pair? token)
          ;(write token)(newline)
          (let ((alternatives (split-alternatives token)))
            (printf " alts: ~S~%" alternatives)
            (find-length rest (+ len (fold (lambda (alt max-len)
                                             (printf " Alt: ~S~%" alt)
                                             (max max-len (find-length alt 0)))
                                           0 alternatives)))))
         (else (error "invalid token"))))))
           
(define input (drop-right (cdr (string->list (read-line))) 2))
(printf "Part 1: ~A~%" (find-length input 0))
