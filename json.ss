;;; JavaScript Object Notation (JSON) Reader/Writer
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

(library (json)
  (export
    json-parser
    json-read
    json-write
    json-do-tests
  )
  (import (rnrs) (peg)
          (only (scheme) fxsll string->number))

  (define fxarithmetic-shift-left
    (lambda (fx1 fx2)
      (fxsll fx1 fx2)))

  (define json-parser
    (peg-parser
      [(obj Object) (pr Pair) (ary Array) (val Value) (str String)
       (num Number) (strchar StringChar) (hd HexDigit) (int Integer)
       (frac Fraction) (exp Exponent) (dgt Digit) (nzdgt NonzeroDigit)
       (ws Whitespace)]
      (Value
        ["true" #t]
        ["false" #f]
        ["null" (if #f #f)]
        [str str]
        [num num]
        [obj obj]
        [ary ary])
      (Object
        [("{" (* ws) (? pr1 (* (* ws) "," (* ws) pr2)) (* ws) "}")
         (cond
           [(peg-unmatched? pr1) '()]
           [(peg-unmatched? pr2) `(,pr1)]
           [else `(,pr1 ,@pr2)])])
      (Pair
        [(str (* ws) ":" (* ws) val) `(,str . ,val)])
      (Array
        [("[" (* ws) (? val1 (* (* ws) "," (* ws) val2)) (* ws) "]")
         (cond
           [(peg-unmatched? val1) (vector)]
           [(peg-unmatched? val2) (list->vector `(,val1))]
           [else (list->vector `(,val1 ,@val2))])])
      (String
        [("\"" (* strchar) "\"")
         (if (peg-unmatched? strchar)
           ""
           (list->string strchar))])
      (StringChar
        [((! (/ "\"" "\\")) (c <- @)) (car c)]
        ["\\\"" #\"]
        ["\\\\" #\\]
        ["\\/" #\/]
        ["\\b" (integer->char 8)]  ; backspace
        ["\\f" (integer->char 12)] ; formfeed
        ["\\n" (integer->char 10)] ; newline
        ["\\r" (integer->char 13)] ; carriage return
        ["\\t" (integer->char 9)]  ; horizontal tab
        [("\\u" hd1 hd2 hd3 hd4)
         (integer->char
           (+ (fxarithmetic-shift-left hd1 24)
              (fxarithmetic-shift-left hd2 16)
              (fxarithmetic-shift-left hd3  8)
              hd4))])
      (HexDigit
        [(digit <- (#\0 - #\9))
         (- (char->integer (car digit)) (char->integer #\0))]
        [(/ "a" "A") 10] [(/ "b" "B") 11]
        [(/ "c" "C") 12] [(/ "d" "D") 13]
        [(/ "e" "E") 14] [(/ "f" "F") 15])
      (Number
        [(int frac exp) (* (+ int frac) (expt 10 exp))]
        [(int frac) (+ int frac)]
        [(int exp) (* int (expt 10 exp))]
        [int int])
      (Integer
        [(nzdgt (+ dgt))
         (string->number (list->string `(,nzdgt ,@dgt)))]
        [dgt (string->number (list->string `(,dgt)))]
        [("-" nzdgt (+ dgt))
         (- (string->number (list->string `(,nzdgt ,@dgt))))]
        [("-" dgt) (- (string->number (list->string `(,dgt))))])
      (Fraction
        [("." (+ dgt))
         (string->number (list->string `(#\. ,@dgt)))])
      (Exponent
        [((/ "e" "e+" "E" "E+") (+ dgt))
         (string->number (list->string dgt))]
        [((/ "e-" "E-") (+ dgt))
         (- (string->number (list->string dgt)))])
      (Digit
        ["0" #\0]
        [nzdgt nzdgt])
      (NonzeroDigit
        [(digit <- (#\1 - #\9)) (car digit)])
      (Whitespace
        [(/ " " "\t" "\r" "\n") #t])))

  (define ~json-read
    (lambda (port/str name line col)
      (define generator
        (cond
          [(string? port/str)
           (let ([str (string->list port/str)])
             (lambda ()
               (if (null? str)
                 (eof-object)
                 (let ([c (car str)])
                   (set! str (cdr str))
                   c))))]
          [(textual-port? port/str)
           (lambda ()
             (read-char port/str))]
          [(binary-port? port/str)
           (let ([port (transcoded-port port/str
                         (make-transcoder (latin-1-codec)))])
             (lambda ()
               (read-char port)))]
          [else
           (error 'json-read "unsupported textual input")]))
      (json-parser (generator->peg-stream generator name line col))))

  (define json-read
    (case-lambda
      [() (~json-read (current-input-port) "<current input port>" 1 1)]
      [(port/str) (~json-read port/str "<?>" 1 1)]
      [(port/str name) (~json-read port/str name 1 1)]
      [(port/str name line col) (~json-read port/str name line col)]))

  (define ~json-write
    (lambda (port object)
      (cond
        [(boolean? object)
         (if object
           (display "true" port)
           (display "false" port))]
        [(number? object)
         (display object port)]
        [(string? object)
         (write object port)]
        [(vector? object)
         (display "[" port)
         (let loop ([i 0])
           (if (< i (vector-length object))
             (begin
               (if (> i 0)
                 (display "," port))
               (~json-write port (vector-ref object i))
               (loop (+ i 1)))))
         (display "]" port)]
        [(list? object)
         (display "{" port)
         (let loop ([elem object] [comma #f])
           (if (not (null? elem))
             (begin
               (if comma
                 (display "," port))
               (~json-write port (caar elem))
               (display ":" port)
               (~json-write port (cdar elem))
               (loop (cdr elem) #t))))
         (display "}" port)]
        [else
         (error 'json-write "unsupported object: ~s" object)])))

  (define json-write
    (case-lambda
      [(object) (~json-write (current-output-port) object)]
      [(object port) (~json-write port object)]))

  (define json-do-tests
    (lambda ()
      (json-read (open-file-input-port "tests/25.js") "tests/25.js" 1 1)
      #f))
)
