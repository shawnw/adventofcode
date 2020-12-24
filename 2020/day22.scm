#!/usr/local/bin/csi -s
(import (chicken format)
        (chicken io)
        (srfi 69)
        (srfi 134) ; Bug in stock egg; see https://github.com/scheme-requests-for-implementation/srfi-134/pull/4
        )
(declare (fixnum-arithmetic) (block))

(define (read-decks #!optional (port (current-input-port)))
  (let loop ((player1 (ideque))
             (player2 (ideque))
             (line (read-line port))
             (which 'player1))
    (cond
     ((eof-object? line) (values player1 player2))
     ((string=? line "") (loop player1 player2 (read-line port) 'player2))
     ((string=? line "Player 1:") (loop player1 player2 (read-line port) 'player1))
     ((string=? line "Player 2:") (loop player1 player2 (read-line port) 'player2))
     (else
      (let ((num (string->number line)))
        (if (eq? which 'player1)
            (loop (ideque-add-back player1 num) player2 (read-line port) 'player1)
            (loop player1 (ideque-add-back player2 num) (read-line port) 'player2)))))))

(define (play player1 player2)
  (cond
   ((ideque-empty? player1) (values 'player2 player2))
   ((ideque-empty? player2) (values 'player1 player1))
   (else
    (let ((p1card (ideque-front player1))
          (p2card (ideque-front player2)))
      (if (> p1card p2card)
          (play (ideque-add-back (ideque-add-back (ideque-remove-front player1)
                                                  p1card)
                                 p2card)
                (ideque-remove-front player2))
          (play (ideque-remove-front player1)
                (ideque-add-back (ideque-add-back (ideque-remove-front player2)
                                                  p2card)
                                 p1card)))))))

(define deck=? (cut ideque= = <> <>))

(define (recplay player1 player2)
  (let ((p1history (make-hash-table deck=? hash))
        (p2history (make-hash-table deck=? hash)))
    (let round ((player1 player1)
                (player2 player2))
      (cond
       ((or (hash-table-exists? p1history player1)
            (hash-table-exists? p2history player2))
        (values 'player1 player1))
       ((ideque-empty? player1)
        (values 'player2 player2))
       ((ideque-empty? player2)
        (values 'player1 player1))
       (else
        (hash-table-set! p1history player1 #t)
        (hash-table-set! p2history player2 #t)
        (let ((p1card (ideque-front player1))
              (p2card (ideque-front player2))
              (new-player1 (ideque-remove-front player1))
              (new-player2 (ideque-remove-front player2)))
          (cond
           ((and (<= p1card (ideque-length new-player1))
                 (<= p2card (ideque-length new-player2)))
            (let-values (((winner deck)
                          (recplay
                           ;;; Work around the ideque-take bug
                           (if (= p1card (ideque-length new-player1))
                               new-player1
                               (ideque-take new-player1 p1card))
                           (if (= p2card (ideque-length new-player2))
                               new-player2
                               (ideque-take new-player2 p2card)))))
              (if (eq? winner 'player1)
                  (round (ideque-add-back (ideque-add-back new-player1 p1card) p2card)
                        new-player2)
                  (round new-player1
                        (ideque-add-back (ideque-add-back new-player2 p2card) p1card)))))
           ((> p1card p2card)
            (round (ideque-add-back (ideque-add-back (ideque-remove-front player1)
                                                     p1card)
                                   p2card)
                  (ideque-remove-front player2)))
           (else
            (round (ideque-remove-front player1)
                  (ideque-add-back (ideque-add-back (ideque-remove-front player2)
                                                    p2card)
                                   p1card))))))))))

(define (score deck)
  (car (ideque-fold-right (lambda (card acc)
                            (cons (+ (car acc) (* card (cdr acc)))
                                  (+ (cdr acc) 1)))
                          (cons 0 1) deck)))

(define (solve1 player1 player2)
  (let-values (((winner deck) (play player1 player2)))
    (score deck)))

(define (solve2 player1 player2)
  (let-values (((winner deck) (recplay player1 player2)))
    (score deck)))

(let-values (((player1 player2) (read-decks)))
  (printf "Part 1: ~A~%" (solve1 player1 player2))
  (printf "Part 2: ~A~%" (solve2 player1 player2)))
