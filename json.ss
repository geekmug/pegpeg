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

;;; References:
;;; RFC 4627, http://www.ietf.org/rfc/rfc4627.txt?number=4627
;;; JSON_checker Test Suite, http://www.json.org/JSON_checker/

(library (json)
  (export
    json-parser
    json-read
    json-read-ex
    json-write
  )
  (import (rnrs) (peg))

  ; RFC requires string to only contain characters in
  ;  {0x20-21, 0x23-5B, 0x5D-10FFFF} 
  (define json-valid-char?
    (lambda (c)
      (or (and (char>=? (integer->char #x20) c)
               (char<=? (integer->char #x21) c))
          (and (char>=? (integer->char #x23) c)
               (char<=? (integer->char #x5B) c))
          (and (char>=? (integer->char #x5D) c)
               (char<=? (integer->char #x10FFFF) c)))))

  (define json-parser
    (peg-parser
      [(obj Object) (pr Pair) (ary Array) (val Value) (str String)
       (num Number) (strchar StringChar) (hd HexDigit) (int Integer)
       (frac Fraction) (exp Exponent) (dgt Digit) (nzdgt NonzeroDigit)
       (ws Whitespace)]
      (Document
        [((* ws) val (* ws)) val])
      (Value
        ["true" #t]
        ["false" #f]
        ["null" (if #f #f)]
        [str str]
        [num num]
        [obj obj]
        [ary ary])
      (Object
        [("{" (* ws) (? pr1 (* (* ws) "," (* ws) pr2)) (? (* ws) ",") (* ws) "}")
         (cond
           [(peg-unmatched? pr1) '()]
           [(peg-unmatched? pr2) `(,pr1)]
           [else `(,pr1 ,@pr2)])])
      (Pair
        [(str (* ws) ":" (* ws) val) `(,str . ,val)])
      (Array
        [("[" (* ws) (? val1 (* (* ws) "," (* ws) val2)) (? (* ws) ",") (* ws) "]")
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
        [((! (/ "\"" "\\")) (c <- %)) (car c)]
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
           (+ (fxarithmetic-shift-left hd1 12)
              (fxarithmetic-shift-left hd2  8)
              (fxarithmetic-shift-left hd3  4)
              hd4))])
      (HexDigit
        [(digit <- ("0" - "9"))
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
        [((/ "e+" "e" "E+" "E") (+ dgt))
         (string->number (list->string dgt))]
        [((/ "e-" "E-") (+ dgt))
         (- (string->number (list->string dgt)))])
      (Digit
        ["0" #\0]
        [nzdgt nzdgt])
      (NonzeroDigit
        [(digit <- ("1" - "9")) (car digit)])
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
           (error 'json-read "unsupported input")]))
      (json-parser (generator->peg-stream generator name line col))))

  (define json-read
    (case-lambda
      [() (~json-read (current-input-port) "<current input port>" 1 1)]
      [(port/str) (~json-read port/str "<?>" 1 1)]
      [(port/str name) (~json-read port/str name 1 1)]
      [(port/str name line col) (~json-read port/str name line col)]))

  (define json-read-ex
    (lambda x
      (let ([result (apply json-read x)])
        (if (peg-parse-error? result)
          (error 'json-read-ex (peg-parse-error-message result))
          result))))

  (define ~json-write
    (lambda (value port)
      (cond
        [(eq? (if #f #f) value)
         (display "null" port)]
        [(boolean? value)
         (if value
           (display "true" port)
           (display "false" port))]
        [(number? value)
         (display value port)]
        [(string? value)
         (write value port)]
        [(vector? value)
         (display "[" port)
         (let loop ([i 0])
           (if (< i (vector-length value))
             (begin
               (if (> i 0)
                 (display "," port))
               (~json-write port (vector-ref value i))
               (loop (+ i 1)))))
         (display "]" port)]
        [(list? value)
         (display "{" port)
         (let loop ([elem value] [comma #f])
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
         (error 'json-write "unsupported scheme value: ~s" value)])))

  (define json-write
    (case-lambda
      [(value) (~json-write value (current-output-port))]
      [(value port) (~json-write value port)]))
)

(library (json tests)
  (export do-tests)
  (import (rnrs) (rnrs eval) (srfi-78) (json))

  (define pass1
"[
    \"JSON Test Pattern pass1\",
    {\"object with 1 member\":[\"array with 1 element\"]},
    {},
    [],
    -42,
    true,
    false,
    null,
    {
        \"integer\": 1234567890,
        \"real\": -9876.543210,
        \"e\": 0.123456789e-12,
        \"E\": 1.234567890E+34,
        \"\":  23456789012E66,
        \"zero\": 0,
        \"one\": 1,
        \"space\": \" \",
        \"quote\": \"\\\"\",
        \"backslash\": \"\\\\\",
        \"controls\": \"\\b\\f\\n\\r\\t\",
        \"slash\": \"/ & \\/\",
        \"alpha\": \"abcdefghijklmnopqrstuvwyz\",
        \"ALPHA\": \"ABCDEFGHIJKLMNOPQRSTUVWYZ\",
        \"digit\": \"0123456789\",
        \"0123456789\": \"digit\",
        \"special\": \"`1~!@#$%^&*()_+-={':[,]}|;.</>?\",
        \"hex\": \"\\u0123\\u4567\\u89AB\\uCDEF\\uabcd\\uef4A\",
        \"true\": true,
        \"false\": false,
        \"null\": null,
        \"array\":[  ],
        \"object\":{  },
        \"address\": \"50 St. James Street\",
        \"url\": \"http://www.JSON.org/\",
        \"comment\": \"// /* <!-- --\",
        \"# -- --> */\": \" \",
        \" s p a c e d \" :[1,2 , 3

,

4 , 5        ,          6           ,7        ],\"compact\":[1,2,3,4,5,6,7],
        \"jsontext\": \"{\\\"object with 1 member\\\":[\\\"array with 1 element\\\"]}\",
        \"quotes\": \"&#34; \\u0022 %22 0x22 034 &#x22;\",
        \"\\/\\\\\\\"\\uCAFE\\uBABE\\uAB98\\uFCDE\\ubcda\\uef4A\\b\\f\\n\\r\\t`1~!@#$%^&*()_+-=[]{}|;:',./<>?\"
: \"A key can be any string\"
    },
    0.5 ,98.6
,
99.44
,

1066,
1e1,
0.1e1,
1e-1,
1e00,2e+00,2e-00
,\"rosebud\"]")
  (define pass2 "[[[[[[[[[[[[[[[[[[[\"Not too deep\"]]]]]]]]]]]]]]]]]]]")
  (define pass3
"{
    \"JSON Test Pattern pass3\": {
        \"The outermost value\": \"must be an object or array.\",
        \"In this test\": \"It is an object.\"
    }
}")
  (define fail1
    "\"A JSON payload should be an object or array, not a string.\"")
  (define fail2 "[\"Unclosed array\"")
  (define fail3 "{unquoted_key: \"keys must be quoted\"}")
  (define fail4 "[\"extra comma\",]")
  (define fail5 "[\"double extra comma\",,]")
  (define fail6 "[   , \"<-- missing value\"]")
  (define fail7 "[\"Comma after the close\"],")
  (define fail8 "[\"Extra close\"]]")
  (define fail9 "{\"Extra comma\": true,}")
  (define fail10
    "{\"Extra value after close\": true} \"misplaced quoted value\"")
  (define fail11 "{\"Illegal expression\": 1 + 2}")
  (define fail12 "{\"Illegal invocation\": alert()}")
  (define fail13 "{\"Numbers cannot have leading zeroes\": 013}")
  (define fail14 "{\"Numbers cannot be hex\": 0x14}")
  (define fail15 "[\"Illegal backslash escape: \\x15\"]")
  (define fail16 "[\\naked]")
  (define fail17 "[\"Illegal backslash escape: \\017\"]")
  (define fail18 "[[[[[[[[[[[[[[[[[[[[\"Too deep\"]]]]]]]]]]]]]]]]]]]]")
  (define fail19 "{\"Missing colon\" null}")
  (define fail20 "{\"Double colon\":: null}")
  (define fail21 "{\"Comma instead of colon\", null}")
  (define fail22 "[\"Colon instead of comma\": false]")
  (define fail23 "[\"Bad value\", truth]")
  (define fail24 "['single quote']")
  (define fail25 "[\" tab character in  string  \"]")
  (define fail26 "[\"tab\\   character\\   in\\  string\\  \"]")
  (define fail27 "[\"line\nbreak\"]")
  (define fail28 "[\"line\\\nbreak\"]")
  (define fail29 "[0e]")
  (define fail30 "[0e+]")
  (define fail31 "[0e+-1]")
  (define fail32 "{\"Comma instead if closing brace\": true,")
  (define fail33 "[\"mismatch\"}")

  (define json-checker-tests
    (lambda ()
      (define safe-eval
        (lambda (x)
          (call/cc
            (lambda (return)
              (with-exception-handler
                (lambda (e)
                  (return 'exception))
                (lambda ()
                  (eval x (environment '(rnrs) '(json)))
                  'no-exception))))))
      (define-syntax check-parse-error
        (lambda (x)
          (syntax-case x ()
            [(_ is-bad input)
             (if (syntax->datum #'is-bad)
               #'(check (safe-eval (list 'json-read-ex input)) => 'exception)
               #'(check (safe-eval (list 'json-read-ex input)) => 'no-exception))])))

      (check-parse-error #f pass1)
      (check-parse-error #f pass2)
      (check-parse-error #f pass3)

      ; Reader was deliberatley relaxed to accept any value.
      (check-parse-error #f fail1)

      (check-parse-error #t fail2)
      (check-parse-error #t fail3)

      ; Reader was deliberatley relaxed to allow trailing commas.
      (check-parse-error #f fail4)

      (check-parse-error #t fail5)
      (check-parse-error #t fail6)

      ; Reader was deliberatley relaxed to ignore trailing junk.
      (check-parse-error #f fail7)
      (check-parse-error #f fail8)

      ; Reader was deliberatley relaxed to allow trailing commas.
      (check-parse-error #f fail9)

      ; Reader was deliberatley relaxed to ignore trailing junk.
      (check-parse-error #f fail10)

      (check-parse-error #t fail11)
      (check-parse-error #t fail12)
      (check-parse-error #t fail13)
      (check-parse-error #t fail14)
      (check-parse-error #t fail15)
      (check-parse-error #t fail16)
      (check-parse-error #t fail17)

      ; Reader has no depth limits.
      (check-parse-error #f fail18)

      (check-parse-error #t fail19)
      (check-parse-error #t fail20)
      (check-parse-error #t fail21)
      (check-parse-error #t fail22)
      (check-parse-error #t fail23)
      (check-parse-error #t fail24)

      ; Reader allows any valid host-language character in a string 
      (check-parse-error #f fail25)

      (check-parse-error #t fail26)

      ; Reader allows any valid host-language character in a string 
      (check-parse-error #f fail27)

      (check-parse-error #t fail28)
      (check-parse-error #t fail29)
      (check-parse-error #t fail30)
      (check-parse-error #t fail31)
      (check-parse-error #t fail32)
      (check-parse-error #t fail33)))

  (define do-tests
    (lambda ()
      (check-set-mode! 'report-failed)
      (json-checker-tests)))
)
