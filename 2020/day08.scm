#!/usr/local/bin/csi -s
(import
 (chicken format)
 (chicken io)
 (srfi 133))
(declare (fixnum-arithmetic) (block))

(define (read-input #!optional (port (current-input-port)))
  (let loop ((input (read-list port))
             (acc '()))
    (if (null? input)
        (reverse-list->vector acc)
        (loop (cddr input) (cons (vector (car input) (cadr input) #f) acc)))))

(define (copy-instructions instructions)
  (vector-map vector-copy instructions))

(define (reset-instructions! instructions)
  (vector-for-each (cut vector-set! <> 2 #f) instructions))

(define (solve1 instructions)
  (let loop ((accumulator 0)
             (ip 0))
    (if (>= ip (vector-length instructions))
        (values #t accumulator)
        (let ((insn (vector-ref instructions ip)))
          (if (vector-ref insn 2)
              (values #f accumulator)
              (begin
                (vector-set! insn 2 #t)
                (case (vector-ref insn 0)
                  ((nop) (loop accumulator (add1 ip)))
                  ((acc) (loop (+ accumulator (vector-ref insn 1)) (add1 ip)))
                  ((jmp) (loop accumulator (+ ip (vector-ref insn 1)))))))))))

(define (flip-and-try instructions ip from to)
  (vector-set! (vector-ref instructions ip) 0 to)
  (let-values (((terminated? acc) (solve1 instructions)))
    (vector-set! (vector-ref instructions ip) 0 from)
    (values terminated? acc)))

(define (solve2 instructions)
  (let* ((default-insns (copy-instructions instructions))
        (executed? (lambda (ip) (vector-ref (vector-ref default-insns ip) 2))))
    (let loop ((ip (sub1 (vector-length instructions))))
      (cond
       ((= ip 0) #f)
       ((not (executed? ip)) (loop (sub1 ip)))
       (else
        (case (vector-ref (vector-ref instructions ip) 0)
          ((acc) (loop (sub1 ip)))
          ((nop jmp) =>
           (lambda (op)
             (reset-instructions! instructions)
             (let*-values (((from to) (if (eq? op 'nop)
                                          (values 'nop 'jmp)
                                          (values 'jmp 'nop)))
                           ((terminated? acc)
                            (flip-and-try instructions ip from to)))
               (if terminated?
                   acc
                   (loop (sub1 ip))))))))))))

(define instructions (read-input))
(let-values (((_ accumulator) (solve1 instructions)))
  (printf "Part 1: ~A~%" accumulator))
(printf "Part 2: ~A~%" (solve2 instructions))
