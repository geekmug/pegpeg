;;; Parsing Expression Grammar Parsing Expression Generator (PEGPEG)
;;; Copyright (c) 2008 Scott A. Dial (scott@scottdial.com)
;;; All rights reserved.
;;;
;;; Redistribution and use in source and binary forms, with or without
;;; modification, are permitted provided that the following conditions
;;; are met:
;;; 1. Redistributions of source code must retain the above copyright
;;;    notice, this list of conditions and the following disclaimer.
;;; 2. Redistributions in binary form must reproduce the above copyright
;;;    notice, this list of conditions and the following disclaimer in the
;;;    documentation and/or other materials provided with the distribution.
;;;
;;; THIS SOFTWARE IS PROVIDED BY THE NETBSD FOUNDATION, INC. AND CONTRIBUTORS
;;; ``AS IS'' AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED
;;; TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR
;;; PURPOSE ARE DISCLAIMED.  IN NO EVENT SHALL THE FOUNDATION OR CONTRIBUTORS
;;; BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
;;; CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
;;; SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
;;; INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
;;; CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
;;; ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
;;; POSSIBILITY OF SUCH DAMAGE.

;;; References:
;;; R. Kent Dybvig, The Scheme Programming Language, Fourth Edition:
;;;     http://www.scheme.com/tspl/
;;; Alessandro Warth, James R. Douglass, Todd Millstein,
;;;     "Packrat Parsers Can Support Left Recursion". Viewpoints Research
;;;     Institute: http://www.vpri.org/pdf/tr2007002_packrat.pdf

(library (peg helpers)
  (export
    peg-trace
    printf
    object->string
    symbol->list
    syntax-symbol=?
    peg-binding-name-match
  )
  (import (rnrs))

  (define dofmt
    (lambda (p cntl args)
      (let ([nmax (- (string-length cntl) 1)])
        (let loop ([n 0] [a args])
          (if (<= n nmax)
              (let ([c (string-ref cntl n)])
                (if (and (char=? c #\~) (< n nmax))
                    (case (string-ref cntl (+ n 1))
                      [(#\a #\A)
                       (display (car a) p)
                       (loop (+ n 2) (cdr a))]
                      [(#\s #\S)
                       (write (car a) p)
                       (loop (+ n 2) (cdr a))]
                      [(#\%)
                       (newline p)
                       (loop (+ n 2) a)]
                      [(#\~)
                       (put-char p #\~) (loop (+ n 2) a)]
                      [else
                       (put-char p c) (loop (+ n 1) a)])
                    (begin
                      (put-char p c)
                      (loop (+ n 1) a)))))))))
  (define printf
    (lambda (control . args)
      (dofmt (current-output-port) control args)))

  (define object->string
    (lambda (object)
      (call-with-string-output-port
        (lambda (port)
          (write object port)))))

  (define symbol->list
    (lambda (sym)
      (string->list (symbol->string sym))))

  (define syntax-symbol=?
    (lambda (stx x)
      (let ([y (syntax->datum stx)])
        (and (symbol? x) (symbol? y)
             (or (free-identifier=? #'x stx)
                 (eq? x y))))))

  (define peg-trace
    (let ([trace #f])
      (case-lambda
        [() trace]
        [(v) (let ([w trace]) (set! trace v) w)])))

  (define peg-binding-name-match
    (lambda (nt-bindings sym)
      (define match?
        (lambda (binding sym)
          (cond
            [(null? binding)
             (or (null? sym)
                 (integer? (string->number (list->string sym))))]
            [(null? sym) #f]
            [(char=? (car binding) (car sym))
             (match? (cdr binding) (cdr sym))]
            [else #f])))
      (let ([symls (symbol->list sym)])
        (call/cc
          (lambda (break)
            (map (lambda (binding)
                   (if (match? (symbol->list (car binding)) symls)
                     (break (cadr binding))))
                 nt-bindings)
            #f)))))
)

(library (peg)
  (export
    peg-parser

    peg-unmatched?

    peg-trace

    generator->peg-stream
    make-peg-stream
    peg-stream?
    peg-stream-value
    peg-stream-next
    peg-stream-name
    peg-stream-line
    peg-stream-col

    peg-parse-error?
    peg-parse-error-suberror
    peg-parse-error-message
    peg-parse-error-stream
  )
  (import (rnrs) (peg helpers))

  (define-record-type peg-stream
    (nongenerative peg-stream-754e2297-a607-4cc3-b384-cadb3a3c670a)
    (fields
      (immutable value) ; value at this position in the stream
      (mutable next)    ; thunk that returns the next peg-stream instance
      (immutable name)  ; name of the stream
      (immutable line)  ; line on which this value appears
      (immutable col)   ; column on which this value appears
      (mutable memo)    ; alist of (nt . result)
    )
    (protocol
      (lambda (new)
        (lambda (value next name line col)
          (new value next name line col '())))))

  (define-record-type peg-result
    (nongenerative peg-result-a5bf48ff-eada-4b56-9fa6-b9aa6c858b62)
    (fields
      (immutable bindings) ; alist of (sym . thunk/group) for body
      (immutable stream) ; continuing peg-stream
    ))

  (define-record-type peg-result-group
    (nongenerative peg-result-group-9323809d-f750-47ba-bc18-bb52cdf37f11)
    (fields
      (mutable list) ; a list of thunks to formed into a normal list
    ))

  ; guard object for optional bindings
  (define-record-type ~peg-unmatched
    (nongenerative ~peg-unmatched-323565f5-91c6-4db1-b0ec-cf5f89a1cd80)
  )
  (define peg-unmatched (make-~peg-unmatched))
  (define peg-unmatched? ~peg-unmatched?)

  (define-record-type peg-body-result
    (nongenerative peg-body-result-22097854-0c44-47c5-8525-6f44dda258e1)
    (fields
      (immutable value)  ; thunk that returns the value of body expression
      (immutable stream) ; continuing peg-stream
    ))

  (define-record-type peg-parse-error
    (nongenerative peg-parse-error-a1067241-5e62-4a94-833c-4e96465a89b3)
    (fields
      (immutable suberror) ; if there is an underlying error for this error
      (immutable message)  ; error message for the user
      (immutable stream)   ; peg-stream location of the error
    ))

  (define generator->peg-stream
    (case-lambda
      [(generator)
       (~generator->peg-stream generator "<?>" 1 1 8)]
      [(generator tab-size)
       (~generator->peg-stream generator "<?>" 1 1 tab-size)]
      [(generator name line col)
       (~generator->peg-stream generator name line col 8)]
      [(generator name line col tab-size)
       (~generator->peg-stream generator name line col tab-size)]))

  (define ~generator->peg-stream
    (lambda (generator name line col tab-size)
      (let ([value (generator)])
        (let ([ps (make-peg-stream value #f name line col)])
          (peg-stream-next-set! ps
            (lambda ()
              (let-values ([(line col)
                            (cond
                              [(eof-object? value) (values line col)]
                              [(char=? value #\newline) (values (+ line 1) 1)]
                              [(char=? value #\tab) (values line (+ col tab-size))]
                              [else (values line (+ col 1 ))])])
                (let ([nps (~generator->peg-stream generator name line col tab-size)])
                  (peg-stream-next-set! ps (lambda () nps))
                  nps))))
          ps))))

  (define peg-stream>?
    (lambda (stream1 stream2)
      (let ([line1 (peg-stream-line stream1)]
            [col1 (peg-stream-col stream1)]
            [line2 (peg-stream-line stream2)]
            [col2 (peg-stream-col stream2)])
        (cond
          [(> line1 line2) #t]
          [(> line2 line1) #f]
          [(> col1 col2) #t]
          [else #f]))))

  (define peg-parse-error>?
    (lambda (err1 err2)
      (let ([stream1 (peg-parse-error-stream err1)]
            [stream2 (peg-parse-error-stream err2)])
        (peg-stream>? stream1 stream2))))

  (define peg-trace-indent
    (let ([indent 0])
      (case-lambda
        [() indent]
        [(v) (set! indent v)])))

  (define peg-trace-print-indent
    (lambda ()
      (let loop ([indent (peg-trace-indent)])
        (if (> indent 0)
          (begin
            (display "| ")
            (loop (- indent 1)))))))

  (define peg-trace-result->string
    (lambda (x)
      (cond
        [(peg-parse-error? x)
         (string-append
           "[peg-parse-error "
           (peg-trace-result->string (peg-parse-error-suberror x))
           " "
           (object->string (peg-parse-error-message x))
           " "
           (peg-trace-result->string (peg-parse-error-stream x))
           "]")]
        [(peg-result? x)
         (string-append
           "[peg-result "
           (object->string (peg-result-bindings x))
           " "
           (peg-trace-result->string (peg-result-stream x))
           "]")]
        [(peg-body-result? x)
         (string-append
           "[peg-body-result "
           (object->string (peg-body-result-value x))
           " "
           (peg-trace-result->string (peg-body-result-stream x))
           "]")]
        [(peg-stream? x)
         (string-append
           "[peg-stream "
           (object->string (peg-stream-value x))
           " "
           (object->string (peg-stream-name x))
           " "
           (object->string (peg-stream-line x))
           " "
           (object->string (peg-stream-col x))
           "]")]
        [else (object->string x)])))

  (define-syntax peg-trace-push
    (lambda (x)
      (syntax-case x ()
        [(_ args ...)
         (if (peg-trace)
           #`(let ()
               (peg-trace-print-indent)
               (printf args ...)
               (peg-trace-indent (+ (peg-trace-indent) 1)))
           #f)])))

  (define-syntax peg-trace-pop
    (lambda (x)
      (syntax-case x ()
        [(_ args ...)
         (if (peg-trace)
           #`(let ()
               (peg-trace-indent (- (peg-trace-indent) 1))
               (peg-trace-print-indent)
               (printf args ...))
           #f)])))

  (define peg-stream-memo-add!
    (lambda (stream k v)
      (let ([memo (peg-stream-memo stream)])
        (peg-stream-memo-set! stream
          (if (assq k memo)
            (map
              (lambda (m)
                (if (eq? (car m) k)
                  (cons k v)
                  m))
              memo)
            `((,k . ,v) ,@memo))))))

  (define peg-stream-memo-fetch
    (lambda (stream k)
      (let ([m (assq k (peg-stream-memo stream))])
        (if m
          (cdr m)
          #f))))

  (define peg-apply-rule
    (lambda (rule stream)
      (let ([m (peg-stream-memo-fetch stream rule)])
        (if (not m)
          (let ([result (rule stream)])
            (peg-stream-memo-add! stream rule result)
            result)
          m))))

  (define-syntax peg-parser
    (lambda (x)
      (define ensure-nt-bindings
        (lambda (nt-bindings)
          (let* ([bindings (syntax->datum nt-bindings)]
                 [binding->stx (map (lambda (d s) (cons d s)) bindings nt-bindings)])
            (map
              (lambda (binding)
                ; ensure binding doesn't end with a number
                (let* ([s (symbol->list (car binding))]
                       [last (list-ref s (- (length s) 1))])
                  (if (and (char>=? last #\0) (char<=? last #\9))
                    (syntax-violation 'peg-parser "invalid binding" x
                      (car (cdr (assq binding binding->stx))))))
                ; ensure binding doesn't match any other binding
                (let ([others (remq binding bindings)])
                  (let ([nt (peg-binding-name-match others (car binding))])
                    (if nt
                      (syntax-violation 'peg-parser "duplicate binding" x
                        (car (cdr (assq binding binding->stx))))))))
              bindings))))
      (syntax-case x ()
        [(_ ((nt-sym* nt-type*) ...) (nt* (nt*-expr* nt*-body** ...) ...) ...)
         (with-syntax ([nt-start
                        (syntax-case #'(nt* ...) ()
                          [(nt0 nt1 ...) #'nt0])])
           (ensure-nt-bindings #'((nt-sym* nt-type*) ...))
           #`(let ()
               (define nt*
                 (lambda (stream)
                   (peg-trace-push "~s~%" 'nt*)
                   (let ([result
                          ((peg-nt ((nt-sym* nt-type*) ...)
                             (nt*-expr* (let () nt*-body** ...)) ...)
                            stream)])
                     (peg-trace-pop "~a~%" (peg-trace-result->string result))
                     result)))
               ...
               (lambda (generator)
                 (let ([result
                        (if (peg-stream? generator)
                          (peg-apply-rule nt-start generator)
                          (peg-apply-rule nt-start
                            (generator->peg-stream generator)))])
                   (if (peg-parse-error? result)
                     result
                     ((peg-body-result-value result)))))))])))

  (define-syntax peg-nt
    (syntax-rules ()
      [(_ nt-bindings (nt-expr nt-body))
       (lambda (stream)
         (let ([result (peg-expr nt-bindings nt-expr stream)])
           (if (peg-parse-error? result)
             result
             (peg-body result nt-expr nt-body))))]
      [(_ nt-bindings (nt-expr0 nt-body0) (nt-expr1 nt-body1) ...)
       (lambda (stream)
         (let ([result0 (peg-expr nt-bindings nt-expr0 stream)])
           (if (peg-parse-error? result0)
             (let ([result1
                    ((peg-nt nt-bindings (nt-expr1 nt-body1) ...) stream)])
               (if (peg-parse-error? result1)
                 (if (peg-parse-error>? result0 result1)
                   result0
                   result1)
                 result1))
             (peg-body result0 nt-expr0 nt-body0))))]))

  (define peg-stream-get-range
    (lambda (start end)
      (let loop ([stream start] [values '()])
        (if (eq? stream end)
          (reverse values)
          (loop ((peg-stream-next stream))
                (cons (peg-stream-value stream) values))))))

  (define peg-result-merge
    (lambda (result1 result2)
      (let ([bindings1 (peg-result-bindings result1)]
            [bindings2 (peg-result-bindings result2)])
        (make-peg-result
          `(,@bindings1 ,@bindings2)
          (peg-result-stream result2)))))

  (define peg-result-append
    (lambda (result1 result2)
      (let ([bindings1 (peg-result-bindings result1)]
            [bindings2 (peg-result-bindings result2)])
        (make-peg-result
          (remq #f (append
            (map (lambda (binding)
                   (let ([key (car binding)] [value (cdr binding)])
                     (let ([entry (assq key bindings2)])
                       (if entry
                         #f
                         `(,key . ,value)))))
              bindings1)
            (map (lambda (binding)
                   (let ([key (car binding)] [value (cdr binding)])
                     (let ([entry (assq key bindings1)])
                       (if entry
                         (let ([group (cdr entry)])
                           `(,key . ,(make-peg-result-group
                                        `(,@(peg-result-group-list group)
                                          ,value))))
                         `(,key . ,(make-peg-result-group `(,value)))))))
              bindings2)))
          (peg-result-stream result2)))))

  (define peg-result-bindings-lookup
    (lambda (result symbol)
      (let ([value (assq symbol (peg-result-bindings result))])
        (if value
          (cdr value)
          peg-unmatched))))

  (define peg-match
    (lambda (stream match? eof-msg fail-msg)
      (let ([v (peg-stream-value stream)])
        (cond
          [(eof-object? v)
           (make-peg-parse-error #f eof-msg stream)]
          [(match? v)
           (make-peg-result '() ((peg-stream-next stream)))]
          [else
           (make-peg-parse-error #f fail-msg stream)]))))

  (define-syntax peg-expr
    (lambda (x)
      (syntax-case x () ;(* + ? & ! / % <- - unquote)
        ; Zero-or-more operator
        [(_ nt-bindings (* nt-expr0 nt-expr1 ...) stream)
         (syntax-symbol=? #'* '*)
         #`(let ([expr (lambda (st)
                         (peg-expr nt-bindings (nt-expr0 nt-expr1 ...) st))])
             (let loop ([nresult (expr stream)]
                        [result (make-peg-result '() stream)])
               (if (peg-parse-error? nresult)
                 result
                 (loop (expr (peg-result-stream nresult))
                       (peg-result-append result nresult)))))]

        ; One-or-more operator
        [(_ nt-bindings (+ nt-expr0 nt-expr1 ...) stream)
         (syntax-symbol=? #'+ '+)
         #`(let ([expr (lambda (st)
                         (peg-expr nt-bindings (nt-expr0 nt-expr1 ...) st))])
             (let ([result (expr stream)])
               (if (peg-parse-error? result)
                 result
                 (let loop ([nresult (expr (peg-result-stream result))]
                            [result (peg-result-append
                                      (make-peg-result '() stream)
                                      result)])
                   (if (peg-parse-error? nresult)
                     result
                     (loop (expr (peg-result-stream nresult))
                           (peg-result-append result nresult)))))))]

        ; Optional operator
        [(_ nt-bindings (? nt-expr0 nt-expr1 ...) stream)
         (syntax-symbol=? #'? '?)
         #`(let ([result
                  (peg-expr nt-bindings (nt-expr0 nt-expr1 ...) stream)])
             (if (peg-parse-error? result)
               (make-peg-result '() stream)
               result))]

        ; And-predicate
        [(_ nt-bindings (& nt-expr0 nt-expr1 ...) stream)
         (syntax-symbol=? #'& '&)
         #`(let ([result
                  (peg-expr nt-bindings (nt-expr0 nt-expr1 ...) stream)])
             (if (peg-parse-error? result)
               result
               (make-peg-result (peg-result-bindings result) stream)))]

        ; Not-predicate
        [(_ nt-bindings (! nt-expr0 nt-expr1 ...) stream)
         (syntax-symbol=? #'! '!)
         #`(let ([result
                  (peg-expr nt-bindings (nt-expr0 nt-expr1 ...) stream)])
             (if (peg-parse-error? result)
               (make-peg-result '() stream)
               (make-peg-parse-error #f
                 #,(string-append
                     "unexpected success of not-predicate: (! "
                     (object->string (syntax->datum #'nt-expr))
                     ")")
                 stream)))]

        ; Ordered-choice operator
        [(_ nt-bindings (/ nt-expr) stream)
         (syntax-symbol=? #'/ '/)
         #`(peg-expr nt-bindings nt-expr stream)]
        [(_ nt-bindings (/ nt-expr0 nt-expr1 ...) stream)
         (syntax-symbol=? #'/ '/)
         #`(let ([result (peg-expr nt-bindings nt-expr0 stream)])
             (if (peg-parse-error? result)
               (peg-expr nt-bindings (/ nt-expr1 ...) stream)
               result))]

        ; Assignment operator
        [(_ nt-bindings (symbol <- nt-expr0 nt-expr1 ...) stream)
         (syntax-symbol=? #'<- '<-)
         #`(let ([result
                  (peg-expr nt-bindings (nt-expr0 nt-expr1 ...) stream)])
             (if (peg-parse-error? result)
               result
               (let ([values (peg-stream-get-range stream
                               (peg-result-stream result))]
                    [bindings (peg-result-bindings result)])
                 (make-peg-result
                   `((symbol . ,(lambda () values)) ,@bindings)
                   (peg-result-stream result)))))]

        ; Range match
        [(_ nt-bindings (start - stop) stream)
         (syntax-symbol=? #'- '-)
         (cond
           [(and (string? (syntax->datum #'start))
                 (string? (syntax->datum #'stop)))
            (let* ([startstr (syntax->datum #'start)]
                   [stopstr (syntax->datum #'stop)]
                   [strrange (string-append
                              (object->string startstr)
                              " - "
                              (object->string stopstr))])
              (let ([startls (string->list startstr)]
                    [stopls (string->list stopstr)])
                (if (or (> (length startls) 1)
                        (> (length stopls) 1)
                        (not (char<=? (car startls) (car stopls))))
                  (syntax-violation 'peg-parser
                    (string-append "invalid match range: " strrange)
                    #'nt-expr)
                  #`(peg-expr nt-bindings
                      (#,(car (string->list startstr)) -
                       #,(car (string->list stopstr))) stream))))]
           [(and (char? (syntax->datum #'start))
                 (char? (syntax->datum #'stop)))
            (let* ([startchar (syntax->datum #'start)]
                   [stopchar (syntax->datum #'stop)]
                   [strrange (string-append
                              (object->string startchar)
                              " - "
                              (object->string stopchar))])
              (if (not (char<=? startchar stopchar))
                (syntax-violation 'peg-parser
                  (string-append "invalid match range: " strrange)
                  #'nt-expr)
                #`(peg-match stream
                    (lambda (v)
                      (and (char>=? v start) (char<=? v stop)))
                    #,(string-append
                        "unexpected end-of-file, expected character in range: "
                        strrange)
                    #,(string-append
                        "unexpected value, expected character in range: "
                        strrange))))]
           [(and (number? (syntax->datum #'start))
                 (number? (syntax->datum #'stop)))
            (let* ([startnum (syntax->datum #'start)]
                   [stopnum (syntax->datum #'stop)]
                   [strrange (string-append
                               (object->string startnum)
                               " - "
                               (object->string stopnum))])
              (if (not (<= startnum stopnum))
                (syntax-violation 'peg-parser
                  (string-append "invalid match range: " strrange)
                  #'nt-expr)
                #`(peg-match stream
                    (lambda (v)
                      (and (>= v start) (<= v stop)))
                    #,(string-append
                        "unexpected end-of-file, expected value in range: "
                        strrange)
                    #,(string-append
                        "unexpected value, expected value in range: "
                        strrange))))]
           [else
            (syntax-violation 'peg-parser
              (string-append "invalid match range: "
                (object->string (syntax->datum #'nt-expr)))
              #'nt-expr)])]

        ; Unquoted match
        [(_ nt-bindings (unquote expr) stream)
         (syntax-symbol=? #'unquote 'unquote)
         #`(let ([val expr])
             (peg-match stream
               (lambda (v)
                 (eqv? v val))
               (string-append
                 #,(string-append
                     "unexpected end-of-file, expected: (unquote "
                     (object->string (syntax->datum #'expr))
                     ") = ")
                 (object->string val))
               (string-append
                 #,(string-append
                     "unexpected value, expected: (unquote "
                     (object->string (syntax->datum #'expr))
                     ") = ")
                 (object->string val))))]

        ; Expand sequences
        [(_ nt-bindings (nt-expr) stream)
         #`(peg-expr nt-bindings nt-expr stream)]
        [(_ nt-bindings (nt-expr0 nt-expr1 ...) stream)
         #`(let ([result0 (peg-expr nt-bindings nt-expr0 stream)])
             (if (peg-parse-error? result0)
               result0
               (let ([result1 (peg-expr nt-bindings (nt-expr1 ...)
                                        (peg-result-stream result0))])
                 (if (peg-parse-error? result1)
                   result1
                   (peg-result-merge result0 result1)))))]

        ; Any match
        [(_ nt-bindings % stream)
         (syntax-symbol=? #'% '%)
         #`(if (eof-object? (peg-stream-value stream))
             (make-peg-parse-error #f
               "unexpected end-of-file, expected any value or character"
               stream)
             (make-peg-result '() ((peg-stream-next stream))))]

        ; Nonterminal or terminal base case
        [(_ nt-bindings nt-expr stream)
         (cond
           ; Expand strings into character sequences
           [(string? (syntax->datum #'nt-expr))
            #`(peg-expr
                nt-bindings
                #,(string->list (syntax->datum #'nt-expr))
                stream)]

           ; Match a character
           [(char? (syntax->datum #'nt-expr))
            #`(peg-match stream
                (lambda (v)
                  (char=? v nt-expr))
                #,(string-append
                    "unexpected end-of-file, expected: "
                    (object->string (syntax->datum #'nt-expr)))
                #,(string-append
                    "unexpected value, expected: "
                    (object->string (syntax->datum #'nt-expr))))]

           ; Match a nonterminal symbol
           [(symbol? (syntax->datum #'nt-expr))
            (let ([nt (peg-binding-name-match
                        (syntax->datum #'nt-bindings)
                        (syntax->datum #'nt-expr))])
              (if (not nt)
                (syntax-violation 'peg-parser
                  (string-append
                    "invalid use of a symbol in PEG expression: "
                    (symbol->string (syntax->datum #'nt-expr)))
                  #'nt-expr)
                #`(let ([result (peg-apply-rule
                                  #,(datum->syntax #'nt-expr nt)
                                  stream)])
                    (if (peg-parse-error? result)
                      (make-peg-parse-error
                        result
                        #,(string-append "unexpected failure of nonterminal: "
                            (symbol->string (syntax->datum #'nt-expr)))
                        (peg-parse-error-stream result))
                      (make-peg-result
                        `((nt-expr . ,(peg-body-result-value result)))
                        (peg-body-result-stream result))))))]

           ; Match a scheme value
           [else
            #`(peg-match stream
                (lambda (v)
                  (eqv? v nt-expr))
                #,(string-append
                    "unexpected end-of-file, expected: "
                    (object->string (syntax->datum #'nt-expr)))
                #,(string-append
                    "unexpected value, expected: "
                    (object->string (syntax->datum #'nt-expr))))])])))

;           ; No other base values are supported
;           [else
;            (syntax-violation 'peg-parser
;              (string-append "invalid PEG expression: "
;                (object->string (syntax->datum #'nt-expr)))
;              #'nt-expr)])])))

  (define-syntax ~peg-body
    (lambda (x)
      (syntax-case x () ;(* + ? & ! / % <- - unquote)
        ; Zero-or-more operator
        [(_ result (* nt-expr0 ...) nt-body)
         (syntax-symbol=? #'* '*)
         #`(~peg-body result (nt-expr0 ...) nt-body)]

        ; One-or-more operator
        [(_ result (+ nt-expr0 ...) nt-body)
         (syntax-symbol=? #'+ '+)
         #`(~peg-body result (nt-expr0 ...) nt-body)]

        ; Optional operator
        [(_ result (? nt-expr0 ...) nt-body)
         (syntax-symbol=? #'? '?)
         #`(~peg-body result (nt-expr0 ...) nt-body)]

        ; And-predicate
        [(_ result (& nt-expr0 ...) nt-body)
         (syntax-symbol=? #'& '&)
         #`(~peg-body result (nt-expr0 ...) nt-body)]

        ; Not-predicate
        [(_ result (! nt-expr0 ...) nt-body)
         (syntax-symbol=? #'! '!)
         #`(~peg-body result (nt-expr0 ...) nt-body)]

        ; Ordered-choice operator
        [(_ result (/ nt-expr0 ...) nt-body)
         (syntax-symbol=? #'/ '/)
         #`(~peg-body result (nt-expr0 ...) nt-body)]

        ; Assignment operator
        [(_ result (symbol <- nt-expr0 ...) nt-body)
         (syntax-symbol=? #'<- '<-)
         #`(let ([symbol (peg-result-bindings-lookup result 'symbol)])
             nt-body)]

        ; Range match
        [(_ result (start - stop) nt-body)
         (syntax-symbol=? #'- '-)
         #'nt-body]

        ; Unquote match
        [(_ result (unquote expr) nt-body)
         (syntax-symbol=? #'unquote 'unquote)
         #'nt-body]

        ; Expand sequences
        [(_ result (nt-expr) nt-body)
         #`(~peg-body result nt-expr nt-body)]
        [(_ result (nt-expr0 nt-expr1 ...) nt-body)
         #`(~peg-body result nt-expr0
             (~peg-body result (nt-expr1 ...) nt-body))]

        ; Any match
        [(_ result % nt-body)
         (syntax-symbol=? #'% '%)
         #'nt-body]

        ; Nonterminal or terminal base case
        [(_ result nt-expr nt-body)
         (if (symbol? (syntax->datum #'nt-expr))
           #`(let ([nt-expr (peg-result-bindings-lookup result 'nt-expr)])
                 nt-body)
           #'nt-body)])))

  (define peg-eval-result
    (lambda (result)
      (define eval-group
        (lambda (group)
          (map (lambda (v)
                 (if (peg-result-group? v)
                   (eval-group v)
                   (v)))
            (peg-result-group-list group))))
      (make-peg-result
        (map (lambda (binding)
               (let ([key (car binding)] [v (cdr binding)])
                 (peg-trace-pop "")
                 (peg-trace-push "<~s>~%" key)
                 (if (peg-result-group? v)
                   `(,key . ,(eval-group v))
                   `(,key . ,(v)))))
          (peg-result-bindings result))
        (peg-result-stream result))))

  (define-syntax peg-body
    (syntax-rules ()
      [(_ result nt-expr nt-body)
       (make-peg-body-result
         (lambda ()
           (peg-trace-push "~s => ~s ~s~%" 'nt-expr 'nt-body
             (peg-result-bindings result))
           (let ([eresult (peg-eval-result result)])
             (let ([bresult (~peg-body eresult nt-expr nt-body)])
               (peg-trace-pop "~s~%" bresult)
               bresult)))
         (peg-result-stream result))]))
)

(library (peg tests)
  (export do-tests)
  (import (rnrs) (rnrs eval) (srfi-78) (srfi-42) (peg))

  (define parse-string
    (lambda (parser input)
      (let ([test-string (string->list input)])
        (define generator
          (lambda ()
            (if (null? test-string)
              (eof-object)
              (let ([c (car test-string)])
                (set! test-string (cdr test-string))
                c))))
        (parser generator))))

  (define expr-tests
    (lambda ()
      (define expr-parser
        (peg-parser
          [(e Expr) (p Product) (v Value) (n Number) (d Digit) (ws Whitespace)]
          (Expr
            [(p1 (* ws) "+" (* ws) p2) (+ p1 p2)]
            [(p1 (* ws) "-" (* ws) p2) (- p1 p2)]
            [p p])
          (Product
            [(v1 (* ws) "*" (* ws) v2) (* v1 v2)]
            [(v1 (* ws) "/" (* ws) v2) (/ v1 v2)]
            [v v])
          (Value
            [("(" (* ws) e (* ws) ")") e]
            [n n])
          (Number
            [(+ d2)
             (let loop ([n 0] [d2 d2])
               (if (null? d2)
                 n
                 (loop (+ (* 10 n) (car d2)) (cdr d2))))])
          (Digit
            [(digit <- ("0" - "9"))
             (- (char->integer (car digit)) (char->integer #\0))])
          (Whitespace
            [(/ " " "\t" "\r" "\n") #t])))
      (check (parse-string expr-parser "222 / 2") => 111)
      (check (parse-string expr-parser "(( (( ((((1)) ))) ) ))") => 1)
      (check (parse-string expr-parser "(22 + 122)*(3 + 2)") => 720)
      (check (parse-string expr-parser "((123+2) / 23)*23 - 1") => 124)
      (check (peg-parse-error? (parse-string expr-parser "((((((((1")) => #t)
      (check (peg-parse-error? (parse-string expr-parser "abc")) => #t)
      (check (peg-parse-error? (parse-string expr-parser "")) => #t)
    ))

  (define abc-tests
    (lambda ()
      ; L = {a^n b^n c^n : n>= 1}
      (define abc-parser
        (peg-parser
          [(a A) (b B)]
          (S
            [((& a (! "b")) (+ "a") b (! "c")) b])
          (A
            [("a" (? a) "b") 'a])
          (B
            [("b" (? b) "c") 'b])))
      (define make-test-string
        (lambda (a b c)
          (string-append
            (string-ec (:range i a) #\a)
            (string-ec (:range i b) #\b)
            (string-ec (:range i c) #\c))))
      (let ([n 10])
        (check-ec (:range a n) (:range b n) (:range c n)
                  (parse-string abc-parser
                    (make-test-string a b c))
          (=> (lambda (p q)
                (if (and (> a 0) (= a b) (= b c))
                  (eqv? p q)
                  (or (peg-parse-error? p) (peg-parse-error? q)))))
          'b))
    ))

  (define syntax-tests
    (lambda ()
      (define safe-eval
        (lambda (x)
          (call/cc
            (lambda (return)
              (with-exception-handler
                (lambda (e)
                  (cond
                    [(syntax-violation? e) (return 'syntax-violation)]
                    [else (return e)]))
                (lambda ()
                  (eval x (environment '(rnrs) '(peg)))))))))
      (define-syntax check-syntax
        (syntax-rules ()
          [(_ is-good x)
           (check (safe-eval x)
             (=> (if is-good
                   (lambda (p q) (not (equal? p q)))
                   equal?))
             'syntax-violation)]))

      ; require a bindings group and at least one valid nt clause
      (check-syntax #f '(peg-parser))
      (check-syntax #f '(peg-parser []))
      (check-syntax #f '(peg-parser [] ()))
      (check-syntax #f '(peg-parser [] (S a)))
      (check-syntax #f '(peg-parser (S ("a" #t))))
      (check-syntax #t '(peg-parser [] (S ("a" #t))))
      (check-syntax #t '(peg-parser [(a S)] (S ("a" #t))))

      ; no duplicate bindings
      (check-syntax #f '(peg-parser [(a S) (a S)] (S ("a" #t))))
      (check-syntax #f '(peg-parser [(a S) (b S) (a S)] (S ("a" #t))))
      (check-syntax #t '(peg-parser [(a S) (b S)] (S ("a" #t))))
      (check-syntax #f '(peg-parser [(a S) (b S) (c S)] (S ("a" #t))))

      ; no bindings that end with digits
      (check-syntax #f '(peg-parser [(a0 S)] (S ("a" #t))))
    ))

  (define do-tests
    (lambda ()
      (check-set-mode! 'report-failed)
      (syntax-tests)
      (abc-tests)
      (expr-tests)
    ))
)
