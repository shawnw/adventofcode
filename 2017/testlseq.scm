(define (make-coroutine-generator proc)
  (define return #f)
  (define resume #f)
  (define yield (lambda (v) (call/cc (lambda (r) (set! resume r) (return v)))))
  (lambda () (call/cc (lambda (cc) (set! return cc)
                        (if resume
                          (resume (if #f #f #f))  ; void? or yield again?
                          (begin (proc yield)
                                 (set! resume (lambda (v) (return (eof-object))))
                                 (return (eof-object))))))))

(define g (make-coroutine-generator
           (lambda (yield)
             (do ((i 0 (+ i 1)))
                 ((= i 100) (eof-object))
               (yield i)))))

(display (g))
(newline)
(display (g))
(newline)
