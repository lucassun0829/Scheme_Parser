; Lexer for Scheme. The following is the lexical specification that it
; handles:
;
; <token> --> <identifier> | <boolean> | <number>
;     | <character> | <string> | ( | ) | #( | ' | ` | , | ,@ | .
; <delimiter> --> <whitespace> | ( | ) | " | ;
; <whitespace> --> <space or newline>
; <comment> --> ; <all subsequent characters up to a line break>
; <atmosphere> --> <whitespace> | <comment>
; <intertoken space> --> <atmosphere>*
;
; <identifier> --> <initial> <subsequent>*
;     | <peculiar identifier>
; <initial> --> <letter> | <special initial>
; <letter> --> [a-z]
;
; <special initial> --> ! | $ | % | & | * | / | : | < | =
;     | > | ? | ^ | _ | ~
; <subsequent> --> <initial> | <digit> | <special subsequent>
; <digit> --> [0-9]
; <special subsequent> --> + | - | . | @
; <peculiar identifier> --> + | - | ...
;
; <boolean> --> #t | #f
; <character> --> #\ <any character> | #\ <character name>
; <character name> --> space | newline
;
; <string> --> " <string element>* "
; <string element> --> <any character other than " or \>
;     | \" | \\
;
; <number> --> <integer> | <decimal>
; <integer> --> <sign> <digit>+
; <decimal> --> <sign> <digit>+ . <digit>*
;     | <sign> . <digit>+
;
; <sign> --> <empty> | + | -
;
; Project UID 7e390a38edc65081bf76ab8edd67fe9d208befb9

(load "distribution.scm")

;;;;;;;;;;;;;;;;;;;

; Read a string token.
(define (read-string)
  (if (read-start #\" "not a string") ; string must start with "
      (read-string-tail '()) ; call helper function below
  )
)

; Read the rest of a string literal.
(define (read-string-tail read-so-far)
  (let ((next-char (get-non-eof-char))) ; read a single char
    (cond ((char=? next-char #\") ; end of string
           ; return a string token
           (token-make 'string (list->string (reverse read-so-far))))
          ((char=? next-char #\\) ; start of escape sequence
           ; read the rest of the escape sequence and recurse
           (read-string-tail (cons (read-escaped) read-so-far)))
          (else
           (read-string-tail (cons next-char read-so-far)))
          ; complete this procedure
    )
  )
)

; Read the rest of an escape sequence.
(define (read-escaped)
  (let ((escaped-char (get-non-eof-char)))
    (if (or (char=? escaped-char #\") (char=? escaped-char #\\))
        escaped-char
        (error "unrecognized escape sequence")
    )
  )
)

;;;;;;;;;;;;;;;;;;;

; Read a boolean token.
(define (read-boolean)
  (if (read-start #\# "not a boolean") ; boolean starts with #
      (read-boolean-tail)
  )
)

; Read the rest of a boolean literal.
(define (read-boolean-tail)
  (let ((next-char (get-non-eof-char)))
    (cond
      ((char=? next-char #\t) (token-make 'boolean #t))
      ((char=? next-char #\f) (token-make 'boolean #f))
      (else (error "not a boolean"))
      )
    )
)

;;;;;;;;;;;;;;;;;;;

; Read a character token.
(define (read-character)
  (if (and (read-start #\# "not a character")  ; character must start
           (read-start #\\ "not a character")) ; with #\
      (read-character-tail)
  )
)

; Read the rest of a character literal.
(define (read-character-tail)
  (let ((char (get-non-eof-char)))
    (cond
      ((char=? char #\newline) (token-make 'character #\newline))
      ((char=? char #\s) (read-character-tail-space))
      ((char=? char #\n) (read-character-tail-newline))
      ((not (delimiter? (peek-char))) (error "character not terminated by delimiter"))
      (else (token-make 'character char))
    )
  )
)

(define (read-character-tail-space)
  (let ((next-char (get-non-eof-char)))
    (if (and (char=? next-char #\p)
             (let ((char (get-non-eof-char))) (char=? char #\a))
             (let ((char (get-non-eof-char))) (char=? char #\c))
             (let ((char (get-non-eof-char))) (char=? char #\e)))
        (token-make 'character #\space)
      (error "not a valid space character")
    )
  )
)
(define (read-character-tail-newline)
  (let ((next-char (get-non-eof-char)))
    (if (and (char=? next-char #\e)
             (let ((char (get-non-eof-char))) (char=? char #\w))
             (let ((char (get-non-eof-char))) (or (char=? char #\l)(char=? char #\L)))
             (let ((char (get-non-eof-char))) (char=? char #\i))
             (let ((char (get-non-eof-char))) (char=? char #\n))
             (let ((char (get-non-eof-char))) (char=? char #\e)))
        (token-make 'character #\newline)
      (error "not a valid newLine character")
    )
  )
)


;;;;;;;;;;;;;;;;;;;

; Determine if the given character is a sign character.
(define (sign? char)
  (or (char=? char #\+) (char=? char #\-))
)

; Determine if the given character is a digit.
(define (digit? char)
  (and (char>=? char #\0) (char<=? char #\9))
)

; Read a number token.
(define (read-number)
  (let ((char (peek-char)))
    (cond ((char=? char #\+) (get-non-eof-char) (read-number-with-sign "+"))
          ((char=? char #\-) (get-non-eof-char) (read-number-with-sign "-"))
          ((char=? char #\.) (get-non-eof-char) (read-number-without-sign "." 1))
          ((digit? char) (read-number-without-sign (string (get-non-eof-char)) 0))
          (else (error "number does not start with digit, +, or -")))))

(define (read-number-with-sign sign)
  (let ((char (peek-char)))
    (cond ((digit? char) (read-number-with-sign-and-integer sign (string (get-non-eof-char)) 0))
          ((char=? char #\.) (get-non-eof-char) (read-number-with-sign-and-decimal sign "." 1))
          (else (error "number does not start with digit")))))

(define (read-number-with-sign-and-integer sign num decimals)
  (let ((char (peek-char)))
    (cond ((digit? char) (read-number-with-sign-and-integer sign (string-append num (string (get-non-eof-char))) decimals))
          ((char=? char #\.) (if (> decimals 0) (error "number cannot contain more than one decimal point")
                                (get-non-eof-char) (read-number-with-decimal (string-append num ".") sign (+ decimals 1))))
          (else (token-make 'number (string->number (string-append sign num)))))))

(define (read-number-with-sign-and-decimal sign num decimals)
  (let ((char (peek-char)))
    (if (digit? char)
        (read-number-with-decimal (string-append num (string (get-non-eof-char))) sign decimals)
        (error "number does not contain a digit after decimal point"))))

(define (read-number-with-decimal num sign decimals)
  (let ((char (peek-char)))
    (if (digit? char)
        (read-number-with-decimal (string-append num (string (get-non-eof-char))) sign decimals)
        (token-make 'number (string->number (string-append sign num))))))


;;;;;;;;;;;;;;;;;;;


; Read an identifier token.
(define (read-identifier)
  (let ((first-char (get-non-eof-char)))
    (if (letter? (char-downcase first-char))
        (let ((str (string (char-downcase first-char))))
          (read-identifier-tail str)
        )
        (error "invlid first char")
    )
  )
)

(define (letter? char)
  (or (char-alphabetic? char)
      (char=? char #\+)
      (char=? char #\-)
      (char=? char #\.)
      (char=? char #\=)
      (char=? char #\?)
  )
)

(define (valid-subsequent-identifier? char)
  (or (char-alphabetic? char)
      (digit? char)
      (char=? char #\+)
      (char=? char #\-)
      (char=? char #\?)
      (char=? char #\=)
      (char=? char #\!)
      (char=? char #\.))
)

(define (read-identifier-tail str)
  (let ((next-char (peek-char)))
    (if (valid-subsequent-identifier? next-char)
        (let ((new_str (string-append str (string (get-non-eof-char)))))
          (if (string=? new_str "...")
              (if (delimiter? (peek-char))
                  (token-make 'identifier (string->symbol new_str))
                  (error "'...' error "))
              (read-identifier-tail new_str)))
        (if (delimiter? (peek-char))
            (token-make 'identifier (string->symbol str))
            (error "Error: bad identifier"))
    )
  )
)
;;;;;;;;;;;;;;;;;;;


; Read a punctuator token (i.e. one of ( ) #( . ' ` , ,@ ).
(define (read-punctuator)
  (let ((char (peek-char)))
    (cond ((char=? char #\() (get-non-eof-char)(token-make 'punctuator "("))
          ((char=? char #\)) (get-non-eof-char)(token-make 'punctuator ")"))
         
          ((char=? char #\#) (get-non-eof-char) 
            (if (char=? (peek-char) #\() 
                (begin(get-non-eof-char) (token-make 'punctuator "#("))
                (token-make 'punctuator "#")
            )
          )
          ((char=? char #\.) (get-non-eof-char)(token-make 'punctuator "."))
          ((char=? char #\') (get-non-eof-char)(token-make 'punctuator "'"))
          ((char=? char #\`) (get-non-eof-char)(token-make 'punctuator "`"))
          ((char=? char #\,) (get-non-eof-char) 
            (if (char=? (peek-char) #\@) 
                (begin(get-non-eof-char) (token-make 'punctuator ",@"))
                (token-make 'punctuator ",")
            )
          )
          (else (error "not a punctuator"))
    )
  )
)

;;;;;;;;;;;;;;;;;;;

; Read a comment. Discards the data and returns an unspecified value.
(define (read-comment)
  (if (read-start #\; "not a comment")
      (read-comment-tail)
  )
)

; Read the rest of a comment.
(define (read-comment-tail)
  (clear-line)
)


;;;;;;;;;;;;;;;;;;;

; Read a token, which can be a boolean, character, string, identifier,
; number, or punctuator. Discards whitespace and comments.
(define (read-token)
  (let ((next-char (peek-char)))
    (cond ((eof-object? next-char) ; eof
           (read-char)) ; just return eof
          ((whitespace? next-char) ; whitespace
           (read-char) ; discard it
           (read-token)) ; read another token
          ((char=? next-char #\;) ; comment
           (read-comment) ; discard it
           (read-token)) ; read another token
          (else
           (let ((char (peek-char)))
             (cond
               ((char=? char #\") (read-string))
               ((char=? char #\#) (get-non-eof-char)(read-hash))
               ((digit? char) (read-number))
               ((sign? char) (read-number))
               ((char=? char #\.) (get-non-eof-char) (read-dot))
               ((or (char=? char #\() (char=? char #\))
                    (char=? char #\') (char=? char #\`)
                    (char=? char #\,))
                (read-punctuator))
               ((or (char=? char #\+) (char=? char #\-))(read-identifier-or-number))
               (else (read-identifier))
             )
           )
         )
      )
  )
)

(define (read-identifier-or-number)
  (let ((char (get-non-eof-char)))
    (cond
      ((digit? (peek-char)) (read-number-with-sign (char->string char)))
      (else (read-identifier-tail (char->string char))))
  )
)


(define (read-hash)
  (let ((char (peek-char))) ; get the next character
    (cond
      ((or (char=? char #\t) (char=? char #\f)) (read-boolean-tail))
      ((char=? char #\\) (get-non-eof-char)(read-character-tail))
      (else (get-non-eof-char )(token-make 'punctuator "#(")) 
    )
  )
)

(define (read-dot)
  (let ((char (peek-char)))
    (cond
      ((digit? char) (read-number-without-sign ".")) ; number starting with .
      ((delimiter? char) (token-make 'punctuator "."))
      (else read-identifier) ; punctuation
    )
  )
)