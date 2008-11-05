(define do-benchmark
  (lambda (thunk)
    (call/cc (lambda (break)
      (let ([suspect (make-engine thunk)]
            [low #f]
            [high #f])
        (collect)
        (set! low (bytes-allocated))
        (set! high (bytes-allocated))
        (let loop ()
          (suspect 100
            (lambda (fuel result)
              (break (- high low)))
            (lambda (engine)
              (set! suspect engine)))
          (let ([v (begin (collect) (bytes-allocated))])
            (if (or (not low) (< v low))
              (set! low v))
            (if (or (not high) (> v high))
              (set! high v)))
          (loop)))))))

(import (json))

(display (do-benchmark (lambda () (json-read (open-file-input-port "800.js") "25.js" 1 1) #t)))
