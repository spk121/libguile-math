(define-module (ryu core)
  #:use-module (ice-9 regex)
  #:use-module (ice-9 i18n)
  #:use-module (ice-9 optargs)
  #:use-module (ice-9 iconv)
  #:use-module (ice-9 rdelim)
  #:use-module (rnrs bytevectors)
  #:use-module (system foreign)
  #:export (;; 5.1.2.2.1 Program startup
	    argc
	    argv

	    ;; 5.2.1.1 Trigraph sequences
	    trigraph->ascii
	    ascii->trigraph

	    ;; 6.5.3.1 Prefix increment and decrement operators
	    c++
	    ++
	    c--
	    --

	    ;; 6.5.3.3 Unary arithmetic operators
	    lognot-uint8
	    lognot-uint16
	    lognot-uint32
	    lognot-uint64

	    ;; 6.5.4 Cast operators
	    cast-int32-to-uint32
	    cast-uint32-to-int32
	    
	    ;; 6.5.5 Multiplicative operators
	    c*
	    c/
	    c%

	    ;; 6.5.6 Additive operators
	    c+
	    c-

	    ;; 6.5.7 Bitwise shift
	    <<
	    >>
	    
	    ;; 6.5.9 Equality operators
	    c==
	    ==
	    c!=
	    !=
	    
	    ;; 6.5.10 Bitwise AND operator
	    bitand

	    ;; 6.5.11 Bitwise exclusive OR operator
	    bitxor

	    ;; 6.5.12 Bitwise inclusive OR operator
	    bitor

	    ;; 6.5.13 Logical AND operator
	    c-logical-and

	    ;; 6.5.14 Logical OR operator
	    c-logical-or

	    ;; 6.5.15 Contitional operator
	    c?:
	    ?:

	    ;; 6.5.16 Assignment operators
	    *=
	    /=
	    %=
	    +=
	    -=
	    <<=
	    >>=
	    &=
	    ^=
	    \|=

	    ;; 6.8.5 Iteration statements
	    for
	    
	    ;; 6.10.8 Predefined macro names
	    __FILE__
	    __LINE__

	    ;; 7.2.1.1 The assert macro
	    cassert

	    ;; 7.3.5.1 The cacos functions
	    cacos

	    ;; 7.3.5.2 The casin function
	    casin

	    ;; 7.3.5.3 The catan function
	    catan

	    ;; 7.3.5.4 The ccos function
	    ccos

	    ;; 7.3.5.5 The csin functions
	    csin
	    
	    ;; 7.4 Character handling
	    locale-uint8->char

	    ;; 7.4.1.1 The isalnum function
	    cisalnum
	    isalnum?

	    ;; 7.4.1.2 The isalpha function
	    cisalpha
	    isalpha?

	    ;; 7.4.1.3 The isblank function
	    cisblank
	    isblank?

	    ;; 7.4.1.4 The iscntrl function
	    ciscntrl
	    iscntrl?

	    ;; 7.4.1.5 The isdigit function
	    cisdigit
	    isdigit?

	    ;; 7.4.1.6 The isgraph function
	    cisgraph
	    isgraph?

	    ;; 7.4.1.7 The islower function
	    cislower
	    islower?

	    ;; 7.4.1.8 The isprint function
	    cisprint
	    isprint?

	    ;; 7.4.1.9 The ispunct function
	    cispunct
	    ispunct?

	    ;; 7.4.1.10 The isspace function
	    cisspace
	    isspace?

	    ;; 7.4.1.11 The isupper function
	    cissupper
	    isupper?

	    ;; 7.4.1.12 The isxdigit function
	    cisxdigit
	    isxdigit?

	    ;; 7.4.2.1 The tolower function
	    ctolower
	    tolower

	    ;; 7.4.2.2 The toupper function
	    ctoupper
	    toupper

	    ;; 7.8.2.1 The imaxabs function
	    imaxabs

	    ;; 7.8.2.2 The imaxdiv function
	    imaxdiv

	    ;; 7.8.2.3 The strtoimax and strtoumax functions
	    strtoimax
	    strtoimax-idx
	    
	    ;; 7.10 Sizes of integer types <limits.h>
	    INT_MIN
	    INT_MAX
	    UINT_MAX
	    LONG_MIN
	    LONG_MAX
	    ULONG_MAX

	    ;; 7.11.2.1 The localeconv function
	    localeconv

	    ;; 7.12.3.2 The isfinite macro
	    cisfinite
	    isfinite?

	    ;; 7.12.3.3 The isinf macro
	    cisinf
	    isinf?

	    ;; 7.12.3.4 The isnan macro
	    cisnan
	    isnan?
	    
	    ;; 7.12.3.6 The signbit macro
	    csignbit
	    signbit
	    
	    ;; 7.12.4.4 The atan2 functions
	    atan2

	    ;; 7.12.6.2 The exp2 functions
	    exp2

	    ;; 7.12.6.3 The expm1 functions
	    expm1

	    ;; 7.12.6.4 The frexp functions
	    frexp

	    ;; 7.12.11.1 The copysign functions
	    copysign

	    ;; 7.18 Boolean type and values
	    ctrue
	    cfalse
	    ->cbool
	    cbool->bool

	    ;; 7.20.2.1 Limits of exact-width integers
	    INT8_MIN
	    INT8_MAX
	    UINT8_MAX
	    INT16_MIN
	    INT16_MAX
	    UINT16_MAX
	    INT32_MIN
	    INT32_MAX
	    UINT32_MAX
	    INT64_MIN
	    INT64_MAX
	    UINT64_MAX
	    
	    ;; 7.21.4.1 The remove function
	    remove

	    ;; 7.21.4.2 The rename function
	    rename
	    
	    ;; 7.21.5.1 The fclose function
	    fclose

	    ;; 7.21.5.2 The fflush function
	    fflush

	    ;; 7.21.5.3 The fopen function
	    fopen

	    ;; <complex.h>
	    CMPLX
	    creal
	    cimag
	    cabs
	    conj
	    cexp
	    clog
	    cpow
	    csqrt
	    csin ccos ctan
	    csinh ccosh ctanh
	    casinh cacosh catanh

	    ;; <math.h>
	    frexp
	    ilogb
	    ldexp
	    log1p
	    log2
	    logb
	    modf
	    scalbn
	    cbrt
	    hypot
	    pow
	    erf
	    lgamma
	    tgamma
	    ceil
	    lround
	    trunc
	    fmod
	    remquo
	    fdim
	    fma
	    
	    ;; <setjmp.h>
	    ;; <signal.h>
	    ;; <stdalign.h>
	    ;; <stdarg.h>
	    ;; <stdatomic.h>

	    ;; <stdint.h>

	    ;; <stdio.h>
	    
	    ;; <stdlib.h>
	    EXIT_SUCCESS
	    EXIT_FAILURE
	    RAND_MAX
	    atof
	    atoi
	    strtol
	    strtol-idx
	    strtof
	    rand
	    srand
	    malloc
	    realloc
	    abort
	    

	    ;; <stdnoreturn.h>
	    ;; <string.h>
	    string-ref-safe
	    strcpy
	    strncpy
	    strncat
	    strlen
	    strnlen
	    strcmp
	    strncmp
	    strcoll
	    strchr
	    strrchr
	    strspn
	    strcspn
	    strpbrk
	    strstr
	    strtok
	    memchr
	    memcmp
	    memset
	    memcpy
	    memmove
	    strerror

	    ;; <tgmath.h>
	    
	    ;; <threads.h>
	    ;; <time.h>
	    ;; <uchar.h>
	    ;; <wchar.h>
	    wctomb
	    mbtowc
	    mbstowcs
	    wcstombs
	    
	    ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 5.1.2.2.1 Program Startup

;; ARGC and ARGV are the count of command-line arguments and pointers
;; to the those arguments.  The core Guile procedure COMMAND-LINE
;; returns the command line arguments as a list of strings.  So
;; approximately
(define (argc)
  "Return the number of command-line arguments for the current program."
  (length (command-line)))

(define* (argv #:optional (n #f))
  "Return the command-line arguments for the current program. If
N is specified, return the Nth command line argument."
  (if n
      (list-ref (command-line) n)
      (command-line)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 5.2.1.1 Trigraph sequences

;; In C, there are trigraphs used for representing C code in
;; environments where the symbols "#[\]^{|}~" may not be available.
;; Guile has no concept of trigraphs.

;; Really, one is never going to see or use trigraphs. I wrote
;; these functions as a joke.

(define *tri:ascii*
  '((#\= . #\#)
    (#\( . #\[)
    (#\/ . #\\)
    (#\) . #\])
    (#\' . #\^)
    (#\< . #\{)
    (#\! . #\|)
    (#\> . #\})
    (#\- . #\~)))

(define *ascii:tri*
  '((#\# . #\=)
    (#\[ . #\()
    (#\\ . #\/)
    (#\] . #\))
    (#\^ . #\')
    (#\{ . #\<)
    (#\| . #\!)
    (#\} . #\>)
    (#\~ . #\-)))

(define (trigraph->ascii input-str)
  "Given a string, return a new string with trigraph sequences
replaced with the corresponding single characters."
  (let ((output-str (string-copy input-str))
	(char-set:tri (string->char-set "=(/)'<!>-")))
    (let loop ((input-index 0)
	       (output-index 0)
	       (state 0))
      (if (< input-index (string-length input-str))
	  (let ((c (string-ref input-str input-index)))
	    (if (eqv? c #\?)
		(cond
		 ((= state 2)
		  ;; We've already seen two question marks.
		  ;; Append one question mark to the output.
		  (string-set! output-str output-index #\?)
		  (loop (1+ input-index)
			(1+ output-index)
			state))
		 ((or (= state 1) (= state 0))
		  ;; We've see 1 question mark so far.  Wait
		  ;; for another one.
		  (loop (1+ input-index)
			output-index
			(1+ state))))
		;; Else, this is not a question mark
		(cond
		 ((and (= state 2) (char-set-contains? char-set:tri c))
		  ;; This is a trigraph.
		  (string-set! output-str output-index
			       (assv-ref *tri:ascii* c))
		  (loop (1+ input-index)
			(1+ output-index)
			0))
		 ((= state 2)
		  ;; This was a couple of question marks followed by
		  ;; a character that doesn't form a trigraph.
		  (string-set! output-str output-index #\?)
		  (string-set! output-str (1+ output-index) #\?)
		  (string-set! output-str (+ 2 output-index) c)
		  (loop (1+ input-index)
			(+ 3 output-index)
			0))
		 ((= state 1)
		  ;; This was a single question mark followed by a
		  ;; character.
		  (string-set! output-str output-index #\?)
		  (string-set! output-str (1+ output-index) c)
		  (loop (1+ input-index)
			(+ 2 output-index)
			0))
		 (else
		  (string-set! output-str output-index c)
		  (loop (1+ input-index)
			(1+ output-index)
			0)))))
	  ;; We've reached the end.  Return the output.
	  (begin
	    ;; There may still be unprocessed question marks that need
	    ;; to be appended.
	    (if (>= state 1)
		(string-set! output-str output-index #\?))
	    (if (= state 2)
		(string-set! output-str (1+ output-index) #\?))
	    (substring output-str 0 (+ output-index state)))))))


(define (ascii->trigraph input-str)
  "Given a string of ASCII test, return a new string, with the
required single characters replaced with trigraph sequences."
  (let* ((char-set:tri (string->char-set "#[\\]^{|}~"))
	 (n (string-count input-str (lambda (c)
					 (char-set-contains?
					  char-set:tri c))))
	 (output-str (make-string (+ (string-length input-str)
					(* 2 n))))
	 (input-length (string-length input-str)))
    (let loop ((input-index 0)
	       (output-index 0))
      (if (>= input-index input-length)
	  output-str
	  ;; else
	  (let ((c (string-ref input-str input-index)))
	    (if (char-set-contains? char-set:tri c)
		(begin
		  (string-set! output-str output-index #\?)
		  (string-set! output-str (1+ output-index) #\?)
		  (string-set! output-str (+ 2 output-index)
			       (assv-ref *ascii:tri* c))
		  (loop (1+ input-index)
			(+ 3 output-index)))
		;; else
		(begin
		  (string-set! output-str output-index c)
		  (loop (1+ input-index)
			(1+ output-index)))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 5.2.2 Character display semantics

;; C has the alphabetic escape sequeces for alert (\a), backspace (\b,
;; form feed (\f), new line (\n), carriage return (\r), horizontal
;; tab (\t), and vertical tab (\v).

;; For Guile, the alphabetic escape sequences for those characters are
;; different, but, for the strings, they are similar.

;; C_ESCAPE   GUILE_CHARACTER   GUILE_STRING_ESCAPE
;; \a         #\alarm           \a
;; \b         #\backspace       \b
;; \f         #\page            \f
;; \n         #\newline         \n
;; \r         #\return          \r
;; \t         #\tab             \t
;; \v         #\vtab            \v

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 6.4.6 Punctuators

;; C has 6 digraph tokens.
;; <: :> <% %> %: and %:%:
;; behave as [ ] { } # and ##

;; But since these need to be in context as tokens, it
;; is not simple to extract them.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 6.5.2.1 Array subscripting

;; In C, the postfix expression in square brackets is a
;; subscripted designation of an array object.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 6.2.5.3 Structure and union members

;; In C, the '.' and '->' operators designates a member
;; of a structure or union object.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 6.5.3.1 Prefix increment and decrement operators

;; ++ -  The increment operator in C can be implemented as
;; a macro in Guile.  But, in Scheme, too much modification of
;; variables is discouraged.
(define-syntax c++
  (syntax-rules ()
    ((_ x)
     (set! x (c+ x 1)))))

(define-syntax ++
  (syntax-rules ()
    ((_ x)
     (set! x (1+ x)))))

(define-syntax c--
  (syntax-rules ()
    ((_ x)
     (set! x (c- x 1)))))

(define-syntax --
  (syntax-rules ()
    ((_ x)
     (set! x (1- x)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 6.5.3.3 Unary arithmetic operators

;; The unary tilde operator in C is the bitwise component of an
;; operand.  In core Guile, LOGNOT is the bitwise component.  But to
;; get behavior like C, you need to decide on the number of bits in
;; the integer.

(define (bytes-to-bits b)
  (* 8 b))

(define (signed-limit b)
  (1- (expt 2 (1- (bytes-to-bits b)))))

(define (unsigned-limit b)
  (1- (expt 2 (bytes-to-bits b))))

(define (signed-limit-neg b)
  (- (expt 2 (1- (bytes-to-bits b)))))

(define-inlinable (lognot-uint x b)
  (- (unsigned-limit b) (logand (unsigned-limit b) x)))

(define (lognot-uint8 x)
  "Find the bitwise complement of an 8-bit unsigned integer."
  (lognot-uint x 1))

(define (lognot-uint16 x)
  "Find the bitwise complement of a 16-bit unsigned integer."
  (lognot-uint x 2))

(define (lognot-uint32 x)
  "Find the bitwise complement of a 32-bit unsigned integer."
  (lognot-uint x 4))

(define (lognot-uint64 x)
  "Find the bitwise complement of a 64-bit unsigned integer."
  (lognot-uint x 8))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 6.5.3.4 The sizeof and _Alignof operators

;; A version of 'sizeof' appears in (system foreign) but only
;; applies to a handful of predefined types.

;; And _ALIGNOF don't have any close analog in core Guile.

;; The unary asterisk in C denotes pointer indirection. There is no
;; close analog to that concept in Guile.  In core Guile, the unary
;; asterisk actually has the meaning of multiplying a value by 1.
;; e.g. (* val) = val.

;; The unary ampersand operator in C denotes taking the address of a
;; variable.  Ther is no close analog to that concept in Guile.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 6.5.4 Cast operators

(define (cast-int32-to-uint32 x)
  (if (< x 0)
      (- #x100000000 (logand #x7fffffff (abs x)))
      (logand #x7FFFFFFF x)))

(define (cast-uint32-to-int32 x)
  (if (<= x #x7fffffff)
      x
      (- (- #x100000000 (logand x #xffffffff)))))

;; No close analog in Guile.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 6.5.5 Multiplicative operators

;; In both C and Guile, when the operands of binary operators have
;; different numerical types, the result will be in the numerical type
;; that is higher in the numerical tower.  If one operand is integer
;; and the other is real, the result will be real.

;; Guile, however, includes the rarely-useful rational types is its
;; numerical tower.

;; Also, C allows math on pointers

(define (c* _a _b)
  "Multiply two values, either numbers or pointers, whilst promoting
any non-integer rational operands to real numbers."
  (let ((a (if (pointer? _a) (pointer-address _a) _a))
	(b (if (pointer? _b) (pointer-address _b) _b)))
    (cond
     ((and (exact-integer? a) (exact-integer? b))
      (* a b))
     (else
      (* (exact->inexact a) (exact->inexact b))))))

(define (c/ _a _b)
  "Divide two values, either numbers or pointers, whilst promoting
any non-integer rational operands to real numbers."
  (let ((a (if (pointer? _a) (pointer-address _a) _a))
	(b (if (pointer? _b) (pointer-address _b) _b)))
    (cond
     ((and (exact-integer? a) (exact-integer? b))
      (quotient a b))
     (else
      (/ (exact->inexact a) (exact->inexact b))))))
   
(define (c% _a _b)
  "Compute the modulo of two values, either numbers or pointers."
  (let ((a (if (pointer? _a) (pointer-address _a) _a))
	(b (if (pointer? _b) (pointer-address _b) _b)))
    (modulo a b)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 6.5.6 Additive operators

(define (c+ _a _b)
  "Add two values, either numbers or pointers, whilst promoting any
non-integer rational operands to real numbers. If _a is a pointer
and _b is an integer, it returns a pointer."
  (cond
   ((and (pointer? _a) (exact-integer? _b))
    (make-pointer (+ (pointer-address _a) _b)))
   (else
    (let ((a (if (pointer? _a) (pointer-address _a) _a))
	  (b (if (pointer? _b) (pointer-address _b) _b)))
      (cond
       ((and (exact-integer? a) (exact-integer? b))
	(+ a b))
       (else
	(+ (exact->inexact a) (exact->inexact b))))))))
  
(define (c- _a _b)
  "Subtract two values, either numbers or pointers, whilst promoting
any non-integer rational operands to real numbers. If _a is a pointer
and _b is an integer, the return value is a pointer."
  (cond
   ((and (pointer? _a) (exact-integer? _b))
    (make-pointer (- (pointer-address _a) _b)))
   (else
    (let ((a (if (pointer? _a) (pointer-address _a) _a))
	  (b (if (pointer? _b) (pointer-address _b) _b)))
      (cond
       ((and (exact-integer? a) (exact-integer? b))
	(- a b))
       (else
	(- (exact->inexact a) (exact->inexact b))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 6.5.7 Bitwise shift operators

;; In C, '<<' and '>>' perform bitwise shift on integer types.  In
;; core Guile, 'ash' is the related procedure.

;; With 'A << B', the operand A is left shifted the indicated number
;; of bit positions, with vacated bits being zero filled.  In C,
;; integers have 1, 2, 4, or 8 bytes, so bits that are shifted off the
;; end are ignored. What happens with negative values of A is
;; undefined.

;; With 'A >> B', A is right shifted B positions.  What happens with
;; negative values of A is also undefined.

;; With core Guile's 'ash' procedure, integers are never left-shifted
;; off, since integers are unbounded.

(define (<< a b)
  "Left shift A by B bits."
  (ash a b))

(define (>> a b)
  "Right-shift A by B bits."
  (ash a (- b)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 6.5.9 Equality operators

;; Guile has many type of equality.

;; C equality returns 0 or 1, not #t or #f
(define (c== _a _b)
  "Given two values, either numbers or pointers, return 1 if A and B
are numerically equal, or zero otherwise, whilst promoting any
non-integer rational operands to real numbers."
  (let ((a (if (pointer? _a) (pointer-address _a) _a))
	(b (if (pointer? _b) (pointer-address _b) _b)))
    (cond
     ((and (exact-integer? a) (exact-integer? b))
      (if (equal? a b) 1 0))
     (else
      (if (equal? (exact->inexact a) (exact->inexact b)) 1 0)))))

(define (== _a _b)
  (equal? _a _b))

(define (c!= _a _b)
  "Given two values, either numbers or pointers, return zero if A and B
are numerically equal, or one otherwise, whilst promoting any
non-integer rational operands to real numbers."
  (let ((a (if (pointer? _a) (pointer-address _a) _a))
	(b (if (pointer? _b) (pointer-address _b) _b)))
    (cond
     ((and (exact-integer? a) (exact-integer? b))
      (if (equal? a b) 0 1))
     (else
      (if (equal? (exact->inexact a) (exact->inexact b)) 0 1)))))

(define (!= _a _b)
  (not (equal? _a _b)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 6.5.10 Bitwise AND operator

;; Bitwise AND operator - called LOGAND in core Guile.
(define (cbitand a b)
  "Compute the bitwise AND of two integers."
  (logand a b))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 6.5.11 Bitwise exclusive OR operator

;; Bitwise exclusive OR operator - called LOGXOR in core Guile.
(define (cbitxor a b)
  "Compute the bitwise exclusive OR of two operands."
  (logxor a b))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 6.5.12 Bitwise inclusive OR operator

;; Bitwise inclusive OR operator - called LOGIOR in core Guile. But
;; the vertical bar has syntactical meaning in Guile, so here it is
;; slash-bar.
(define (cbitor a b)
  "Compute the bitwise OR of two integers"
  (logior a b))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 6.5.13 Logical AND operator

;; Converting logical operations from C to Guile must handle how C
;; uses non-zero and zero integers for true and false, whilst Guile
;; has a boolean type.

(define (c-logical-and a b)
  "Compute the logical AND of two scalar operands, returning 1 if operands
compare unequal to zero, and zero otherwise."
  (if (and (not (zero? a)) (not (zero? b)))
      1
      0))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 6.5.14 Logical OR operator

;; Logical OR operator - called 'or' in core Guile. But in C, the
;; exact integer zero indicates false.  Here we create a hybrid
;; between C and Guile where both #f and 0 are false.
(define (c-logical-or a b)
  "Compute the logical or of two scalar operands, returning 1 if either operand
compares unequal to zero, and zero otherwise."
  (if (or (not (zero? a)) (not (zero? b)))
      1
      0))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 6.5.15 Conditional operator

;; The '?:' ternary operator in C returns the second operand if the
;; first operand is unequal to zero. Otherwise it returns the third
;; operand.  It is analgous to the core Guile's 'if; procedure.  Here
;; we create a hybrid between C and Guile where both #f and 0 are
;; false.
(define (c?: test on-success on-failure)
  "If test is unequal to zero, return on-success.
Else return on-failure."
  (if (not (zero? test))
      on-success
      on-failure))

(define (?: test on-success on-failure)
  "If test is unequal to #f, return on-success.
Else return on-failure."
  (if test
      on-success
      on-failure))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 6.5.16 ASSIGNMENT OPERATORS

;; These are = *= /= %= += -= <<= >>= &= ^= and |\

;; As always, Scheme programmers aren't big on assignment.
;; They'll tell you that you shouldn't be assigning variables
;; unless there is no other option.

;; Even so, here are some analagous syntax.

;; Plain old equal assignment is set! in core Guile.

(define-syntax +=
  (syntax-rules ()
    ((_ x a)
     (set! x (c+ x a)))))

(define-syntax -=
  (syntax-rules ()
    ((_ x a)
     (set! x (c- x a)))))

(define-syntax *= 
  (syntax-rules ()
    ((_ x a)
     (set! x (c* x a)))))

(define-syntax /= 
  (syntax-rules ()
    ((_ x a)
     (set! x (c/ x a)))))

(define-syntax %=
  (syntax-rules ()
    ((_ x a)
     (set! x (c% x a)))))

(define-syntax &= 
  (syntax-rules ()
    ((_ x a)
     (set! x (bitand x a)))))

(define-syntax \|= 
  (syntax-rules ()
    ((_ x a)
     (set! x (bitor x a)))))

(define-syntax ^= 
  (syntax-rules ()
    ((_ x a)
     (set! x (bitxor x a)))))

(define-syntax <<=
  (syntax-rules ()
    ((_ x a)
     (set! x (<< x a)))))

(define-syntax >>=
  (syntax-rules ()
    ((_ x a)
     (set! x (>> x a)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 6.5.17 Comma operator

;; The C operator ',', is analagous to the core Guile procedure
;; 'begin'.  The operands are evaluated left-to-right, and the last
;; one is returned.

;; But, in C, the comma is also syntax used to separated items in a
;; list.

;; In Guile, the comma is the unquote operator.

;; So use 'begin' for comma.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 6.7.1 Storage-class specifiers

;; In C, a declaration has either an explicit or implied storage-class
;; specifier, and it is one of: 'extern', 'static', '_Thread_local',
;; 'auto', or 'register'

;; There is no easy one-to-one.

;; To create a thread-local storage, use a fluid.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 6.7.2.4 Atomic type specifiers

;; In C, the _Atomic type specifier can indicate that a declared
;; variable can be accessed by multiple threads, and that operations
;; on atomic variable will occur one at a time, and never
;; simultaneously by different threads.

;; In Guile, 'make-atomic-box', 'atomic-box-ref', and 'atomic-box-set!'
;; can be used.

;; To define a function or variable at module level, use 'define'

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 6.7.4 Function specifiers

;; In C, the 'inline' function specifier indicates that calls
;; to an inline function can, at compile time, be replaced with
;; the code for that function itself.

;; To define an 'inline' procedure in Guile, use 'define-inlinable'

;; There is no analog for the '_Noreturn' function specifier in Guile.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 6.7.5 Alignment specifier

;; Typically in Guile, one doesn't control the details of how
;; objects are stored.  There is no easy analog to C's '_Alignas'
;; specifier.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 6.7.6 Declarators

;; In Guile, 'define' or 'let' are the basic declaration
;; mechanisms.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 6.8.4.1 The 'if' statement

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 6.8.4.2 The 'switch' statement

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 6.8.5 Iteration statements

;; The C iteration statements are 'while', 'do' and 'for'.

;; Guile is big on telling you that named 'let' and recursion
;; is the right thing to do.  But many named 'let' loops are difficult
;; to interpret by those that didn't write them.

;; Sometimes, I use Guile's 'do', which is as close as one can get to
;; a C 'for' loop in core Guile.


;; A simple 'for' loop for a single variable.
;; call like this
;; (for (i 0) (< i 10) (1+ i)
;;      ...)
;; It supports 'break' and 'continue' syntax.

(define-syntax for
  (lambda (x)
    (syntax-case x ()
      ((for (init-var init-val) cond iterator body ...)
       #`(let ((break-tag (make-prompt-tag "break"))
               (continue-tag (make-prompt-tag "continue")))
           (call-with-prompt
               break-tag
             (lambda ()
               (define-syntax #,(datum->syntax #'for 'break)
                 (lambda (x)
                   (syntax-case x ()
                     ((_ arg (... ...))
                      #'(abort-to-prompt break-tag arg (... ...)))
                     (_
                      #'(lambda args
                          (apply abort-to-prompt break-tag args))))))
               (let lp ()
                 (call-with-prompt
                     continue-tag
                   (lambda () 
                     (define-syntax #,(datum->syntax #'for 'continue)
                       (lambda (x)
                         (syntax-case x ()
                           ((_)
                            #'(abort-to-prompt continue-tag))
                           ((_ . args)
                            (syntax-violation 'continue "too many arguments" x))
                           (_
                            #'(lambda ()
                                (abort-to-prompt continue-tag))))))
                     (do ((init-var init-val iterator)) ((not cond) #f) body ...))
                   (lambda (k) (lp)))))
             (lambda (k . args)
               (if (null? args)
                   #t
                   (apply values args)))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 6.8.6 Jump statements

;; The C jump statements are 'goto', 'continue', 'break',
;; 'return'.

;; The best analog to 'goto' in core Guile is
;; call-with-current-configuration, aka call/cc, but, they
;; are very different concepts in practice.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 6.10 Preprocessing directives

;; Any attempt at multiplatform C is littered with
;; prepreocessing directives like '#if' '#ifdef' '#ifndef'
;; '#elif' '#else' and '#endif'.

;; There is also '#include' '#define' '#undef' '#line'
;; '#error' and '#pragma'

;;;;
;; For '#if' '#ifdef' '#ifndef' '#elif' '#else' and '#endif'.

;; There isn't really a Guile preprocessor, as such.

;; The SRFI-0 procedure 'cond-expand' provides conditionals
;; different versions of guile, and the presence of different
;; SRFI's, so this may help replace #ifdef for code that is
;; dependent on different versions of guile.

;; There is also the core Guile 'provided?' procedure that can
;; check if certain platform-dependent features are available.

;; Lastly, the M4 macro processor can be used on Guile, if one
;; really more features than 'cond-expand', 'include' and
;; 'provided?' provide.

;; For '#include'

;; In core Guile is an 'include' and 'include-from-path' which
;; acts much like #include does. It inserts the contents of
;; another file at compile time.

;; For including a file only a run-time, 'load' and 'load-from-path'
;; will work.

;; There is also the standard 'use-modules' directive.

;; Be aware of the cases that may require a call to 'eval-when'.

;; For '#define' of macros

;; In modern C++, they encourage inline functions over defines
;; where possible.  Thus for a #define replacement, consider
;; core Guile's 'define-inlineable'.

;;;;
;; For '#line'

;; In C '#line' is a pre-processor directive that indicates
;; the line number of the current source line.  Theoretically,
;; once could mess about with core Guile's 'set-source-properties!',
;; but it doesn't seem useful.

;;;;
;; For '#error'

;; In C, "#error" is used to terminate compilation. Usually,
;; it appears within certain combinations of #if and #ifdef
;; directives.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 6.10.8 Predefined macro names

;; In C, there compile-time predefined macros include
;; __FILE__, __LINE__, __DATE__, and __TIME__.

;; For compiled Guile objects, the file name and line number
;; may appear in their source properties.  Here are a couple
;; of analagous procedures that may work, depending on how
;; the code was interpreted or compiled.
(define-syntax __FILE__
   (syntax-rules ()
     ((_)
      (or (assv-ref (current-source-location) 'filename)
	  "(unknown file)"))))

(define-syntax __LINE__
   (syntax-rules ()
     ((_)
      (or (assv-ref (current-source-location) 'line)
	  "(unknown line)"))))

;; There is also the core Guile 'current-filename' procedure.


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 7.2.1.1 The assert macro

;; If expression compares equal to zero, the assert macro writes
;; information about the call that failed, and then calls abort.


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 7.3.5.1 The cacos functions

(define (cacos x) (acos x))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 7.3.5.2 The casin functions

(define (casin x) (asin x))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 7.3.5.3 The catan function

(define (catan x) (atan x))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 7.3.5.4 The ccos function

(define (ccos x) (cos x))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 7.3.5.5 The csin functions

(define (csin x) (sin x))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 7.3.5.6 The ctan function

(define (ctan x) (tan x))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 7.3.6.1 The cacosh function

(define (cacosh x) (acosh x))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 7.3.6.2 The casinh functions

(define (casinh x) (asinh x))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 7.3.6.3 The catanh functions

(define (catanh x) (atanh x))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 7.3.6.4 The ccosh functions

(define (ccosh x) (cosh x))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 7.3.6.5 The csinh functions

(define (csinh x) (sinh x))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 7.3.6.6 The ctanh function

(define (ctanh x) (tanh x))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 7.3.7.1 The cexp functions

(define (cexp x) (exp x))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 7.3.7.2 The clog functions

(define (clog x) (log x))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 7.3.8.1 The cabs functions

(define (cabs x) (magnitude x))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 7.3.8.2 The cpow functions

(define (cpow x y) (expt x y))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 7.3.8.3 csqrt functions

(define (csqrt x) (sqrt x))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 7.3.9.1 The carg functions
(define (carg z) (angle z))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 7.3.9.2 The cimag functions

(define (cimag z)
  "Computes the imaginary part of a complex number."
  (imag-part z))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 7.3.9.3 The CMPLX macros

(define-syntax CMPLX
  (syntax-rules ()
    ((_ x y)
     (make-rectangular x y))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 7.3.9.4 The conj function

(define (conj z)
  "Computes the complex conjugate of z."
  (- (real-part z) (imag-part z)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 7.3.9.5 The cproj function

(define (cproj z)
  "Computes the projection fo z onto the Riemann sphere.
That is, returns z, unless z is infinite."
  (cond
   ((or (inf? (real-part z)) (inf? (imag-part z)))
    (make-rectangular +inf.0 (imag-part z)))
   (else
    z)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 7.3.9.6 The creal function

(define (creal z)
  "Computes the real part of a complex number."
  (real-part z))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 7.4 8-bit Character handling

;; The functions is ctype are for either 8-bit characters or the EOF
;; value, and they are affected by the current locale.

(define-inlinable (%cchar->char x)
  (false-if-exception
   (string-ref (bytevector->string (make-bytevector 1 x)
				   (locale-encoding %global-locale)
				   'error)
	       0)))

(define-inlinable (%char->cchar x)
  (let ((bv (false-if-exception
	     (string->bytevector (string x)
				 (locale-encoding %global-locale)
				 'error))))
    (if (and bv (= (bytevector-length bv) 1))
	(bytevector-u8-ref bv 0)
	#f)))

(define (locale-uint8->char x)
  "Given an 8-bit integer, return a character.  Make
the conversion using the current locale. Return #f
on failure."
  (if (and (exact-integer? x)
	   (<= 0 x)
	   (<= x UINT8_MAX))
      (%cchar->char x)
      #f))

(define-inlinable (%cchar x)
  (cond
   ((char? x)
    x)
   ((and (exact-integer? x) (<= 0 x) (<= x UINT8_MAX))
    (%cchar->char x))
   (else
    #f)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 7.4.1.1 The isalnum function
(define (cisalnum _c)
  "Return 1 if the 8-bit integer C maps to an alphanumeric character
in the current locale, else 0"
  (let ((c (%cchar _c)))
    (if (and c (char-set-contains? char-set:letter+digit c))
	1 0)))

(define (isalnum? c)
  "Return #t if the character C is alphanumeric."
  (char-set-contains? char-set:letter+digit c))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 7.4.1.2 The isalpha function

(define (cisalpha _c)
  "Return 1 if the 8-bit integer C maps to an alphabetic character
in the current locale, else 0"
  (let ((c (%cchar _c)))
    (if (and c (char-set-contains? char-set:letter c))
	1 0)))

(define (isalpha? c)
  "Return #t if the character C is alphabetic."
  (char-set-contains? char-set:letter c))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 7.4.1.3 The isblank function

(define (cisblank _c)
  "Return 1 if the 8-bit integer C maps to an blank character
in the current locale, else 0"
  (let ((c (%cchar _c)))
    (if (and c (char-set-contains? char-set:blank c))
	1 0)))

(define (isblank? c)
  "Return #t if the character C is alphabetic."
  (char-set-contains? char-set:blank c))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 7.4.1.4 The iscntrl function

(define (ciscntrl _c)
  "Return 1 if the 8-bit integer maps C is a control character, else 0."
  (let ((c (%cchar _c)))
    (if (and c (char-set-contains? char-set:iso-control c))
	1 0)))

(define (iscntrl? c)
  "Return true if C an ISO C0 or C1 control character"
  (char-set-contains? char-set:iso-control c))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 7.4.1.5 The isdigit function
(define (cisdigit _c)
  "Return 1 if the 8-bit integer maps C is a decimal digit character, else 0."
  (let ((c (%cchar _c)))
    (if (and c (char-set-contains? char-set:digit c))
	1 0)))

(define (isdigit? c)
  "Return #t if the character C is a digit, else #f"
  (char-set-contains? char-set:digit c))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 7.4.1.6 The isgraph function

(define (cisgraph _c)
  "Return 1 if the 8-bit integer maps C is a printing character, else 0."
  (let ((c (%cchar _c)))
    (if (and c (char-set-contains? char-set:graphic c))
	1 0)))

(define (isgraph? c)
  "Return #t if the character C is a printing character, else #f"
  (char-set-contains? char-set:graphic c))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 7.4.1.7 The islower function

(define (cislower _c)
  "Return 1 if the 8-bit integer C maps to a small character, else 0."
  (let ((c (%cchar _c)))
    (if (and c (char-set-contains? char-set:lower-case c))
	1 0)))

(define (islower? c)
  "Return #t if C a lowercase letter."
  (char-set-contains? char-set:lower-case c))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 7.4.1.7 The isprint function

(define (cisprint  _c)
  "Return 1 if the 8-bit integer C maps to a printing character, else 0."
  (let ((c (%cchar _c)))
    (if (and c (char-set-contains? char-set:printing c))
	1 0)))

(define (isprint? c)
  "Return #t if C a printing character."
  (char-set-contains? char-set:printing c))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 7.4.1.8 The ispunct function

(define (cispunct  _c)
  "Return 1 if the 8-bit integer C maps to a punctuation character, else 0."
  (let ((c (%cchar _c)))
    (if (and c (char-set-contains? char-set:punctuation c))
	1 0)))

(define (ispunct? c)
  "Return #t if C a punctuation character."
  (char-set-contains? char-set:punctuation c))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 7.4.1.10 The isspace function

(define (cisspace  _c)
  "Return 1 if the 8-bit integer C maps to a whitespace character, else 0."
  (let ((c (%cchar _c)))
    (if (and c (char-set-contains? char-set:whitespace c))
	1 0)))

(define (isspace? c)
  "Return #t if C is a whitespace character."
   (char-set-contains? char-set:whitespace c))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 7.4.1.11 The isupper function

(define (cisupper _c)
  "Return 1 if the 8-bit integer C maps to an uppercase character, else 0."
  (let ((c (%cchar _c)))
    (if (and c (char-set-contains? char-set:upper-case c))
	1 0)))

(define (isupper? c)
  "Return #t if C is an uppercase character."
   (char-set-contains? char-set:upper-case c))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 7.4.1.12 The isxdigit function

(define (cisxdigit _c)
  "Return 1 if the 8-bit integer C maps to a hexadecimal-digit
character, else 0."
  (let ((c (%cchar _c)))
    (if (and c (char-set-contains? char-set:hex-digit c))
	1 0)))

(define (isxdigit? c)
  "Return #t if C is a whitespace character."
   (char-set-contains? char-set:hex-digit c))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 7.4.2.1 The tolower function

(define (ctolower _c)
  "Return the 8-bit integer representation of the lower-case letter
to which the 8-bit integer maps."
  (let* ((c (%cchar _c))
	 (L (and c (char-downcase c))))
    (%char->cchar L)))

(define (tolower c)
  (char-downcase c))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 7.4.2.2 The toupper function

(define (ctoupper _c)
  "Return the 8-bit integer representation of the upper-case letter
to which the 8-bit integer maps."
  (let* ((c (%cchar _c))
	 (L (and c (char-downcase c))))
    (%char->cchar L)))

(define (toupper c)
  (char-downcase c))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 7.5 Errors <errno.h>

;; ERRNO - There isn't really a good way to directly access
;; the underlying errno.

;; E2BIG, EACCESS, ..., EXDEV - most of these contstants
;; exist in core Guile.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 7.6 Floating-point environment <fenv.h>

;; Almost everyting in <fenv.h> has no analog in core Guile.
;; Access to the details of floating point numbers is hidden.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 7.7 Characteristics of floating types <float.h>

;; Details about the floating point numbers in core Guile is
;; supposed to be hidden.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 7.8 Format conversion of integger types <inttypes.h>

;; Some of these format specifiers might be useful for
;; a fully specified printf command

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 7.8.2.1 The imaxabs function

(define (imaxabs j)
  "Compute the absolute value of integer j."
  (abs j))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 7.8.2.2 The imaxdiv function

(define (imaxdiv numer denom)
  "Return a pair containing the numerator and the denominator
of the integer division of the variables."
  (let ((ratio (/ numer denom)))
    (cons (numerator ratio) (denominator ratio))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 7.8.2.3 The strtoimax and strtoumax functions

(define* (strtoimax-idx _str #:optional (base 0))
  "Convert STR to an integer, assuming BASE. BASE is either 0, or 2 to
36, with a default value of 0.

If BASE is 0, the number will assumed to be base-16 if it starts with
0x or 0X, base 8 if it starts with 0, or base 10 otherwise.

Will return a a pair.  The car is the integer value, or zero if no
conversion can be returned.  The cdr is the number of characters
processed."
  (strtol-idx _str base))

(define* (strtoimax _str #:optional (base 0))
  "Convert STR to an integer, assuming BASE. BASE is either 0, or 2 to
36, with a default value of 0.

If BASE is 0, the number will assumed to be base-16 if it starts with
0x or 0X, base 8 if it starts with 0, or base 10 otherwise.

Will return an integer value, or zero if no conversion can be
returned."
  (car (strtol-idx _str base)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 7.9 Alternative spellings <iso646.h>

;; This syntactic sugar is rarely used.
;; 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 7.10 Sizes of integer types <limits.h>

;; Details about the limits of integers isn't often relevant
;; in Guile.

(define INT_MIN (signed-limit-neg (sizeof int)))
(define INT_MAX (signed-limit (sizeof int)))
(define UINT_MAX (unsigned-limit (sizeof unsigned-int)))
(define LONG_MIN (signed-limit-neg (sizeof long)))
(define LONG_MAX (signed-limit (sizeof long)))
(define ULONG_MAX (unsigned-limit (sizeof unsigned-long)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 7.11 Localization <locale.h>

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 7.11.1.1 The setlocale function

;; SETLOCALE - there is a good SETLOCALE in core Guile

;; LC_ALL, LC_COLLATE, LC_CTYPE, LC_MONETARY, LC_NUMERIC,
;; and LC_TIME are all defined in core Guile.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 7.11.2.1 The localeconv function

;; LOCALECONV - in C, this returns a pointer to a structure
;; with locale information.  Of course, pointers to structures
;; don't translate well to Guile.  One could return a list
;; of locale information, like in the procedure below.
(define (localeconv)
  "Return a list that contains numeric and monetary formatting
rules for the current locale."
  (let ((loc %global-locale))
    (list
     (cons 'decimal-point (locale-decimal-point))
     (cons 'thousands-separator (locale-thousands-separator loc))
     (cons 'grouping (locale-digit-grouping loc))
     (cons 'mon-decimal-point (locale-monetary-decimal-point loc))
     (cons 'mon-thousands-sep (locale-monetary-thousands-separator loc))
     ;;(cons 'mon-grouping (locale-monetary-grouping loc))
     (cons 'positive-sign (locale-monetary-positive-sign loc))
     (cons 'negative-sign (locale-monetary-negative-sign loc))
     
     (cons 'currency-symbol (locale-currency-symbol #f loc))
     (cons 'monetary-fractional-digits
	   (locale-monetary-fractional-digits #f loc))
     (cons 'p-cs-precedes 
	   (locale-currency-symbol-precedes-positive? #f loc))
     (cons 'n-cs-precedes 
	   (locale-currency-symbol-precedes-negative? #f loc))
     (cons 'p-sep-by-space
	   (locale-positive-separated-by-space? #f loc))
     (cons 'n-sep-by-space
	   (locale-negative-separated-by-space? #f loc))`
     (cons 'p-sign-posn (locale-monetary-positive-sign loc))
     (cons 'n-sign-posn (locale-monetary-negative-sign loc))
     
     (cons 'int-curr-symbol (locale-currency-symbol #t loc))
     (cons 'int-frac-digits
	   (locale-monetary-fractional-digits #t loc))
     (cons 'int-p-cs-precedes 
	   (locale-currency-symbol-precedes-positive? #t loc))
     (cons 'int-n-cs-precedes 
	   (locale-currency-symbol-precedes-negative? #t loc))
     (cons 'int-p-sep-by-space
	   (locale-positive-separated-by-space? #t loc))
     (cons 'int-n-sep-by-space
	   (locale-negative-separated-by-space? #t loc))`
     (cons 'int-p-sign-posn (locale-monetary-positive-sign loc))
     (cons 'int-n-sign-posn (locale-monetary-negative-sign loc)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 7.12.3.2 The isfinite macro

(define (cisfinite x)
  "Return 1 if X is finite, else 0."
  (if (finite? x) 0 1))

(define (isfinite? x)
  "Return #t if X is finite, else #f."
  (finite? x))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 7.12.3.3 The isinf macro

(define (cisinf x)
  "Return 1 if X is infinite, else 0."
  (if (inf? x) 1 0))

(define (isinf? x)
  "Return #t if X is infinite, else #f."
  (inf? x))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 7.12.3.4 The isnan macro

(define (cisinf x)
  "Return 1 if X is +nan.0, else 0."
  (if (nan? x) 1 0))

(define (isnan? x)
  "Return #t if X is +nan.0, else #f."
  (nan? x))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 7.12.3.5 The isnormal macro

(define (cisnormal x)
  "Return 1 is X is neither zero, subnormal, infinite, or NaN.
Else 0."
  (if (and (not (zero? x))
	   (not (inf? x))
	   (not (nan? x)))
      1
      0))

(define (isnormal? x)
  "Return #t if the number X is neither zero, subnormal, infinite,
or NaN.  Else #f"
  (and (not (zero? x))
       (not (inf? x))
       (not (nan? x))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 7.12.3.6 The signbit macro

(define (csignbit x)
  "Return 1 if x is negative. Else 0."
  (if (< x 0) 1 0))

(define (signbit x)
  "Return #t if x is negative."
  (if (< x 0)
      #t
      #f))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 7.12.4.1 The acos functions
;; 7.12.4.2 The asin functions
;; 7.12.4.3 The atan functions
;; 7.12.4.4 The atan2 functions

(define (atan2 y x)
  "Computes the arctangent of y/x."
  (atan y x))

;; 7.12.4.5 The cos functions
;; 7.12.4.6 The sin functions
;; 7.12.4.7 The tan functions
;; 7.12.5.1 The acos functions
;; 7.12.5.2 The asinh functions
;; 7.12.5.3 The atanh functions
;; 7.12.5.4 The cosh functions
;; 7.12.5.5 The sinh functions
;; 7.12.5.6 The tanh functions
;; 7.12.6.1 The exp functions
;; 7.12.6.2 The exp2 functions

(define (exp2 x)
  "Compute the base-2 exponential of x."
  (expt 2.0 x))

;; 7.12.6.3 The expm1 functions

(define (expm1 x)
  "Computes the base-e exponential of the argument, minus 1."
  (- (exp x) 1.0))

;; 7.12.6.4 The frexp functions

;; FIXME: slow and accumulates multiplication errors
(define (frexp val)
  "Returns val broken as a normalized fraction and an integral power
of 2."
  (if (zero? val)
      (cons 0 0)
      ;; 
      (let loop ((val val)
		 (x   0))
	(cond
	 ((< val 0.5)
	  (loop (* val 2.0) (1- x)))
	 ((>= val 1.0)
	  (loop (* val 0.5) (1+ x)))
	 (else
	  (cons val x))))))

;; 7.12.11.1 The copysign functions

(define (copysign x y)
  "Return a value with the magnitude of X and the sign of Y. Y should not be
complex."
  (* (magnitude x)
     (if (< 0 y)
	 -1
	 1)))

;; 7.18 Boolean type and values
(define ctrue 1)

(define cfalse 0)

(define (->cbool x)
  "Returns 0 if x if #f, else it returns 1."
  (if x 1 0))

(define (cbool->bool x)
  "Returns #f if x is zero. Else returns #t."
  (if (zero? x) #f #t))

;; 7.20.2.1 Limits of exact-width integers

(define INT8_MIN (signed-limit-neg (sizeof int8)))
(define INT8_MAX (signed-limit (sizeof int8)))
(define UINT8_MAX (unsigned-limit (sizeof uint8)))

(define INT16_MIN (signed-limit-neg (sizeof int16)))
(define INT16_MAX (signed-limit (sizeof int16)))
(define UINT16_MAX (unsigned-limit (sizeof uint16)))

(define INT32_MIN (signed-limit-neg (sizeof int32)))
(define INT32_MAX (signed-limit (sizeof int32)))
(define UINT32_MAX (unsigned-limit (sizeof uint32)))

(define INT64_MIN (signed-limit-neg (sizeof int64)))
(define INT64_MAX (signed-limit (sizeof int64)))
(define UINT64_MAX (unsigned-limit (sizeof uint64)))

(define LONG_MIN (signed-limit-neg (sizeof long)))
(define LONG_MAX (signed-limit (sizeof long)))
(define ULONG_MAX (unsigned-limit (sizeof unsigned-long)))

;; 7.21.4.1 The remove function
;; The remove function in C is the same as the delete-file
;; function in core Guile.  There is an SRFI-1 'remove'
;; which operates on lists.
(define (remove fname)
  "Delete the file whose path is specified by FNAME."
  (delete-file fname))

;; 7.21.4.2 The rename function
(define (rename old new)
  "Rename causes the file whose name is the string OLD to 
be known by the name NEW."
  (rename-file old new))

;; 7.21.4.3 The tmpfile function
;; Returns a file pointer opened for update in "wb+" mode
;; and that will be closed if the program terminates normally.

;; 7.21.4.4 The tmpnam function
;; In core Guile

;; 7.21.5.1 The fclose function
(define (fclose port)
  (if (not (file-port? port))
      (error "not a file port: ~a" port)
      (begin
	(when (output-port? port)
	  (force-output port))
	(close-port port))))
  
;; 7.21.5.2 The fflush function
;; Causes any unwritten data to be delivered.
(define (fflush port)
  (if (not port)
      (flush-all-ports)
      ;; else
      (begin
	(if (not (file-port? port))
	    (error "not a file port: ~a" port))
	(when (output-port? port)
	  (force-output port)))))

;; 7.21.5.3 The fopen function
(define (fopen filename mode)
  "Open a file port assuming the current global locale."
  (open-file filename mode #:encoding (locale-encoding
				       %global-locale)))

;; 7.21.5.4 The freopen function
;; 7.21.5.5 The setbuf function
;; 7.21.5.6 The setvbuf function

;; There is one in core Guile.

;; 7.21.6.1 The fprintf functions


;; 7.21.6 Formatted input/output
#!
(define FLAG_GROUP 1)
(define FLAG_LEFT 2)
(define FLAG_SHOWSIGN 4)
(define FLAG_SPACE 8)
(define FLAG_ALT 16)
(define FLAG_ZERO 32)
(define FLAG_LOCALIZED 64)

(define ARG_NONE -1)

(define-record-type <format-directive>
  (make-format-directive dir-start dir-end flags width-start width-end
			 width-arg-index precision-start precision-end
			 precision-arg-index conversion arg-index)
  format-directive?
  dir-start
  dir-end
  flags
  width-start
  width-end
  width-arg-index
  precision-start
  precision-end
  precision-arg-index
  conversion
  arg-index)

(define (new-format-directive index)
  (make-format-directive index ARG_NONE ARG_NONE ARG_NONE ARG_NONE
			 ARG_NONE ARG_NONE ARG_NONE ARG_NONE))

(define (populatate-directives str)
  (let loop ((index 0))
    (if (not (eqv? #\null (string-ref-safe str index)))
	(let ((dp (new-format-directive)))))))
!#

;; 7.21.6.2 The fscanf function
;; 7.21.6.3 The printf function
;; 7.21.6.4 The scanf function
;; 7.21.6.5 The sprintf function
;; 7.21.6.6 The sprintf function
;; 7.21.6.7 The sscanf function
;; 7.21.6.8 The vfprintf function
;; 7.21.6.9 the vfscanf function
;; 7.21.6.10 The vprintf function
;; 7.21.6.11 The vscanf function
;; 7.21.6.12 The vsnprintf function
;; 7.21.6.13 The vsprintf function
;; 7.21.6.14 The vsscanf function
;; 7.21.7.1 The fgetc function
(define (fgetc port)
  (read-char port))

;; 7.21.7.2 The fgets function
(define (fgets str n port)
  "Read a line of text from port. Copy that text into the string STR.
If the line of text is greater than n characters, only n characters
are copied."
  (let ((x (read-line port 'concat)))
    (string-copy! str 0 x 0 (min n (string-length x)))))



;; 7.21.8.1 The fread function
;; 7.21.8.2 The fwrite function
;; 7.21.9.1 The fgetpos function
;; 7.21.9.2 The fseek function
;; 7.21.9.3 The fsetpos function

;; 7.21.9.4 The ftell function
;; This is in core Guile

;; 7.21.9.5 The rewind function
(define (rewind fd_port)
  "Sets the current position of a FD_PORT to the beginning of the
file."
  (seek fd_port 0 SEEK_SET))

;; 7.21.10.1 The clearerr function

;; 7.21.10.2 The feof function
;; Returns the end of file indicator for a stream.

;; 7.21.10.3 The ferror function
;; The ferror function returns non-sero if the error indicator
;; is set for a given FILE *stream.

;; 7.21.10.4 The perror function
;; There is no good way to access the current errno, so
;; it is hard to implement perror.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; <stdlib.h>

;; 7.22 
(define RAND_MAX 2147483647)

(define EXIT_SUCCESS 0)
(define EXIT_FAILURE 1)

;; 7.22.1.1 The atof function

(define (atof str)
  "Convert STR into a real number if STR begins with a base-10 real
number in the C locale representation. Initial whitespace is
ignored. Return 0.0 if STR does not begin with a number.  Any
characters after the number are ignored."
  (car (strtod str)))

;; 7.22.1.2 The atoi, atol, and atoll functions

(define (atoi _str)
  "Search the beginning of _str for a base-10 integer.  Return
the value if found, or #f otherwise."
  (car (strtol _str 10)))

;; 7.22.1.3 The strtod, strtof, and strtold functions
(define *integer-match*
  (make-regexp "^[-+]?[0-9]+"))

(define *infinity-match*
  (make-regexp
   "^[-+]?(INF|INFINITY)" regexp/icase))

(define *nan-match*
  (make-regexp
   "^[-+]?(NAN|NAN\\([_a-z0-9]+\\))" regexp/icase))

;; A regular expression for a base-10 floating point that may use 'e'
;; or 'E' as the exponent indicator.
(define *floating-point-match*
  (make-regexp
   "^[-+]?([0-9]+(\\.[0-9]*)?|\\.[0-9]+)(e[-+]?[0-9]+)?" regexp/icase))

(define *hex-floating-point-match*
  (make-regexp
   "^[-+]?0x([0-9a-f]+(\\.[0-9a-f]*)?|\\.[0-9a-f]+)(p[-+]?[0-9]+)?" regexp/icase))

(define (remove-zero-x str)
  "Strip a '0x' or '0X' from a potential hexadecimal number."
  (cond
   ((eqv? #\+ (string-ref str 0))
    (string-drop str 3))
   ((eqv? #\- (string-ref str 0))
    (string-append "-" (string-drop str 3)))
   (else
    (string-drop str 2))))

(define (hexstr->number _str)
  "Convert a string containing a hexadecimal floating point number
with optional exponent into a number."
  (let ((str (string-copy _str))
	(sign 1)
	(significand 0)
	(fractional 0)
	(exponent 0))
    ;; First, there may be a sign
    (let ((c (string-ref str 0)))
      (cond
       ((eqv? #\+ c)
	(set! sign 1)
	(set! str (string-drop str 1)))
       ((eqv? #\- c)
	(set! sign -1)
	(set! str (string-drop str 1)))))
    ;; Next, there is either a decimal point or the integer part of
    ;; the significand.
    (let ((decimal-point-location (string-contains str "."))
	  (exponent-location (string-contains-ci str "p"))
	  (significand-str "")
	  (fractional-str "")
	  (exponent-str ""))
      (cond
       ((and (not decimal-point-location) (not exponent-location))
	(set! significand-str str))
       ((and decimal-point-location (not exponent-location))
	(set! significand-str (substring str 0 decimal-point-location))
	(set! fractional-str (substring str (1+ decimal-point-location))))
       ((and (not decimal-point-location) exponent-location)
	(set! significand-str (substring str 0 exponent-location))
	(set! exponent-str (substring str (1+ exponent-location))))
       ((and decimal-point-location exponent-location)
	(set! significand-str (substring str 0 decimal-point-location))
	(set! fractional-str (substring str (1+ decimal-point-location) exponent-location))
	(set! exponent-str (substring str (1+ exponent-location)))))

      ;; Extract the numeric parts.
      ;; If there is no significand in the string, use zero.
      (set! significand (or (string->number significand-str 16) 0))

      ;; These may be #f if they don't appear
      (set! fractional (string->number fractional-str 16))
      (set! exponent (string->number exponent-str 10))

      (cond
       ((and (not fractional) (not exponent))
	;; Return an integer value
	(* sign significand))
       ((and fractional (not exponent))
	;; Return a floating point value
	(* sign (+ significand
		   (/ fractional
		      (expt 16 (string-length fractional-str))))))
       ((and (not fractional) exponent)
	;; Integer
	(* sign significand (expt 10 exponent)))
       (else
	;; Floating point
	(* sign
	   (+ significand
	      (/ fractional (expt 16 (string-length fractional-str))))
	   (expt 10 exponent)))))))

(define (strtod str)
  "Convert STR into a real number, returning both the number
and a string containing the unused characters after the number.
The number is one of the the following forms
- an optional plus or minus followed by INF or INFINITY, ignoring case 
- an optional plus or minus followed by NAN, ignoring case.
- A string of the form NAN(<type>), where <type> is a string containing
  alphanumeric or hyphen characters.
- a base-10 decimal floating point
- a base-16 float starting with 0x or 0X and using p or P as
  the exponent separator."
  (let ((s (string-trim str)))
    (if (string-null? s)
	(cons 0.0 (substring s 0))
	(let ((match1 (regexp-exec *hex-floating-point-match* s)))
	  (if match1
	      (cons (or (hexstr->number (remove-zero-x (match:substring match1))) 0.0)
		    (substring s (match:end match1)))
	      (let ((match2 (regexp-exec *floating-point-match* s)))
		(if match2
		    (cons (or (string->number (match:substring match2)) 0.0)
			  (substring s (match:end match2)))
		    (let ((match3 (regexp-exec *infinity-match* s)))
		      (if match3
			  (cons (inf)
				(substring s (match:end match3)))
			  (let ((match4 (regexp-exec *nan-match* s)))
			    (if match4
				(cons (nan)
				      (substring s (match:end match4)))
				(cons 0.0
				      (substring s 0)))))))))))))
	

;; 7.22.1.4 The strtol, strtoll, strtoul, strtoull functions

(define (string-ref-safe str i)
  "A version of string-ref that returns a null character
if the index is out of bounds."
  (if (< i (string-length str))
      (string-ref str i)
      #\null))

(define* (strtol _str #:optional (base 0))
  "Convert STR to an integer, assuming BASE. BASE is either 0, or 2 to
36, with a default value of 0.

If BASE is 0, the number will assumed to be base-16 if it starts with
0x or 0X, base 8 if it starts with 0, or base 10 otherwise.

Will return an integer value, or zero if no conversion can be
returned."
  (car (strtol-idx _str base)))

(define* (strtol-idx _str #:optional (base 0))
  "Convert STR to an integer, assuming BASE. BASE is either 0, or 2 to
36, with a default value of 0.

If BASE is 0, the number will assumed to be base-16 if it starts with
0x or 0X, base 8 if it starts with 0, or base 10 otherwise.

Will return a a pair.  The car is the integer value, or zero if no
conversion can be returned.  The cdr is the number of characters
processed."
  (let ((str (string-trim _str)))
    (if (string-null? str)
	;; String is empty; quit now.
	(cons 0 0)

	(let ((i 0)
	      (neg #f))
	  ;; Look for a Guile style initializer of #x, #o #d or
	  ;; #b, maybe followed by plus or minus.
	  ;; FIXME
	  (if (char=? (string-ref-safe str i) #\#)
	      'fixme)
	  ;; Otherwise,
	  ;; Look for a C style initializer of plus or minus maybe
	  ;; followed by 0x for hex.
	  (if (char=? (string-ref-safe str i) #\-)
	      (begin
		(set! neg #t)
		(set! i (1+ i)))
	      (if (char=? (string-ref-safe str i) #\+)
		  (set! i (1+ i))))
	  (if (and (= base 0)
		   (char=? (string-ref-safe str i) #\0)
		   (char-ci=? (string-ref-safe str (1+ i)) #\x))
	      (begin
		(set! i (+ i 2))
		(set! base 16)))
	  (if (and (= 0 base)
		   (char=? (string-ref str i) #\0))
		  (set! base 8))
	  (if (= base 0)
	      (set! base 10))
	  (let loop ((c (string-ref-safe str i))
		     (i i)
		     (acc 0))
	    (let ((val (cond
			((and (char>=? c #\0) (char<=? c #\9))
			 (- (char->integer c) (char->integer #\0)))
			((and (char>=? c #\A)  (char<=? c #\Z))
			 (+ 10 (- (char->integer c) (char->integer #\A))))
			((and (char>=? c #\a)  (char<=? c #\z))
			 (+ 10 (- (char->integer c) (char->integer #\a))))
			(else
			 #f))))
	      (if (or (not val) (>= val base))
		  ;; return
		  (cons (if neg (- acc) acc) i)
		  ;; else continue
		  (begin
		    (loop (string-ref-safe str (1+ i))
			  (1+ i)
			  (+ (* acc base) val))))))))))

;; 7.22.2.1 The rand function
(define *local-random-state* #f)

(define (rand)
  "Retun a random number between 0 and RAND_MAX"
  (if *local-random-state*
      (random RAND_MAX *local-random-state*)
      (random (1+ RAND_MAX))))

;; 7.22.2.2 The srand function

(define (srand x)
  "See the random state used by 'rand'."
  (set! *local-random-state* (seed->random-state x)))

;; 7.22.3.1 The aligned_alloc function

;; 7.22.3.2 The calloc function
(define (calloc size1 size2)
  "Return a bytevector, initialize to zero whose size
is size1 * size2."
  (make-bytevector (* size1 size2) 0))
  
;; 7.22.3.3 The free function

;; 7.22.3.4 The malloc function

(define (malloc len)
  "Return a bytevector containing SIZE bytes, whose values are not
specified."
  (make-bytevector len))

;; 7.22.3.5 The realloc function

(define (realloc bv len)
  "Return a new bytevector of length LEN, with the contents set
to the contents of the bytevector BV.  If BV is shorter than LEN,
the extra bytes in BV will be undetermined."
  (let ((bv2 (make-bytevector len)))
    (bytevector-copy! bv 0 bv2 0 (min (bytevector-length bv) len))
    bv2))

;; 7.22.4.1 The abort function

(define (abort)
  "This procedure causes abnormal program termination to occur,
unless the SIGABRT is being caught and the signal handler does not
return."
  (raise SIGABRT)
  (primitive-_exit 1))

;; 7.22.4.2 The atexit function

;; 7.24.4.3 The at_quick_exit function

;; 7.22.4.4 The exit function

;; 7.22.4.5 The _Exit function

;; 7.22.4.6 The getenv function

;; 7.22.4.7 The quick_exit function

;; 7.22.4.8 The system function

;; 7.22.5.1 The bsearch function

;; 7.22.5.2 The qsort function

;; 7.22.6.1 The abs, labs, and llabs functions

;; 7.22.6.2 The div, ldiv, lldiv functions

(define (div num denom)
  (cons (quotient num denom)
	(remainder num denom)))

;; 7.22.7.1 The mblen function
;; FIXME: horribly inefficient.
(define (mblen mbs)
  "Compute the number of bytes required to extract the first
representation of a character in the current locale, or #f if the
first few bytes do not form a character in the current locale."
  (let ((enc (locale-encoding %global-locale))
	(maxlen (min MB_LEN_MAX (bytevector-length mbs))))
  (let loop ((len 1))
    (if (> len maxlen)
	#f
	;; else
	(let ((bv (make-bytevector len)))
	  (bytevector-copy! mbs 0 bv 0 len)
	  (let ((str (false-if-exception (bytevector->string bv enc 'error))))
	    (if (and str (> (string-length str) 0))
		len
		;; else, didn't convert, so keep looping
		(loop (1+ len)))))))))
  
(define MB_LEN_MAX 8)

;; 7.22.7.2 The mbtowc function
;; FIXME: horribly inefficient.  Copy-pasta with mblen.
(define (mbtowc mbs)
  "Converts the first multibyte character byte sequence in the
bytevector into a corresponding character, interpreting the bytevector
bytes as an encoded string in the current locale.  It returns #f on a
conversion error."
  (let ((enc (locale-encoding %global-locale))
	(maxlen (min MB_LEN_MAX (bytevector-length mbs))))
  (let loop ((len 1))
    (if (> len maxlen)
	#f
	;; else
	(let ((bv (make-bytevector len)))
	  (bytevector-copy! mbs 0 bv 0 len)
	  (let ((str (false-if-exception (bytevector->string bv enc 'error))))
	    (begin
	      (write str) (newline)
	      (if (and str (> (string-length str) 0))
		  (string-ref str 0)
		  ;; else, didn't convert, so keep looping
		  (loop (1+ len))))))))))

;; 7.22.7.3 The wctomb function
(define (wctomb wc)
  "Converts a character to a bytevector sequence of bytes that
represents the character in the current locale.  It returns #f on a
conversion error"
  (false-if-exception
   (string->bytevector (string wc)
		       (locale-encoding %global-locale)
		       'error)))

;; 7.22.8.1 The mbstowcs function
(define (mbstowcs mbs)
  "Converts the bytevector sequence of bytes mbs into a string and
returns the string.  It uses the current locale's encoding. It returns
#f on a conversion error."
  (false-if-exception
   (bytevector->string mbs (locale-encoding %global-locale) 'error)))

;; The wcstombs function

(define (wcstombs wcs)
  "Converts the string SRC to a locale-encoded bytevector in the
current global locale and returns the bytevector.  It returns #f on a
conversion error."
  (false-if-exception
   (string->bytevector wcs (locale-encoding %global-locale) 'error)))
    

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; <stdnoreturn.h>

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; <string.h>

;; 7.24.2.1 The memcpy function
;; MEMCPY copies a block of raw memory from one location to another.
;; If we're using bytevector to hold raw blocks, it could be implemented
;; like the following.
(define (memcpy dest src count)
  (let ((end (min count (bytevector-length src) (bytevector-length dest))))
    (bytevector-copy! src 0 dest 0 end)
    dest))

;; 7.24.2.2 The memmove function
;; MEMMOVE copies a block of raw memory from one location to another, just
;; like MEMCPY, but, being smart about overlapping regions.  But since
;; it is quite difficult to create overlapping bytevectors, MEMCPY

;; 7.24.2.3 The strcpy function

(define (strcpy dest src)
  "Copies the contents of string DEST into string SRC.  If DEST is
shorter than SRC, then only (string-length DEST) characters of SRC are
morified.  If DEST is longer than SRC, then the characters in DEST
that will not fit in SRC are ignored.

Note that if SRC is not a writable string, such as created with
'string-copy' this procedure will fail."
  (let ((srclen (string-length src))
	(destlen (string-length dest)))
    (substring-move! src 0 (min srclen destlen) dest 0)
    dest))

;; 7.24.2.4 The strncpy function

(define (strncpy dest src count)
  "Copies at most COUNT characters of the string SRC into DEST."
  (let ((srclen (string-length src))
	(destlen (string-length dest)))
    (substring-move! src 0 (min srclen destlen count) dest 0)
    (if (and (< srclen count) (<= count destlen))
	(substring-fill! dest srclen count #\null))
    dest))

;; 7.24.3.1 The strcat function
;; strcat doesn't really work, since a string's storage is
;; the same as a string's length.  You can't copy past the end
;; of it.

;; 7.24.3.2 The strncat function
;; Ditto with strncat.

;; 7.24.4.1 The memcmp function
;; MEMCMP is a binary comparison between two raw locations in memory.
;; If we're using bytevector to hold raw blocks, it could be
;; implemented like the following.
(define (memcmp a b count)
  "Compare the first COUNT bytes in bytevectors A and B, and return
-1, 0, or 1."
  (let ((alen (bytevector-length a))
	(blen (bytevector-length b)))
    (let loop ((i 0))
      (cond
       ((>= i count)
	0)
       ((>= i alen)
	-1)
       ((>= i blen)
	1)
       (else
	(let ((ach (bytevector-u8-ref a i))
	      (bch (bytevector-u8-ref b i)))
	  (cond
	   ((< ach bch)
	    -1)
	   ((> ach bch)
	    1)
	   (else
	    (loop (1+ i))))))))))

;; 7.24.4.2 The strcmp function 
(define (strcmp a b)
  "Compares two strings lexicographically, returning -1, 0, or 1,
if A is less than, equal, or greater than B."
  (cond
   ((string<? a b)
    -1)
   ((string>? a b)
    1)
   (else
    0)))

;; 7.24.4.3 The strcoll function
;; Compares two strings using the current LC_COLLATE category
;; of the current locale, and returns -1, 0, or 1.
(define (strcoll a b)
  "Compares two strings using the current locale's collating rules,
 returning -1, 0, or 1, if A is less than, equal, or greater than B."
  (cond
   ((string-locale<? a b)
    -1)
   ((string-locale>? a b)
    1)
   (else
    0)))

;; 7.24.4.4. The strncmp function
(define (strncmp a b n)
  (strcmp (substring a 0 (min n (string-length a)))
	  (substring b 0 (min n (string-length b)))))

;; 7.24.4.5 The strxfrm function
;; strxfrm has no good analog with Guile strings.

;; 7.24.5.1 The memchr function

(define (memchr-idx bv c n)
  "Returns an index to the first location of the 8-bit unsigned byte C
in the bytevector BV.  If C doesn't occur in the first N bytes of BV,
#f is returned."
  (let ((maxlen (min (bytevector-length bv) n))
	(c2 (if (char? c)
		(char->integer c)
		c)))
    (let loop ((i 0))
      (if (>= i maxlen)
	  #f
	  ;; else
	  (if (eqv? c2 (bytevector-u8-ref bv i))
	      i
	      ;; else
	      (loop (1+ i)))))))
	      
;; 7.24.5.2 The strchr function
;; STRCHR doesn't really translate because it returns a pointer to the
;; first character, but, a good approimation is
;; (string-index STR CHAR)

(define (strchr-idx str c)
  (string-index str c))

;; 7.24.5.3 The strcspn function
(define (strcspn dest src)
  "Returns the length of the maximum initial segment of the DEST, that
consists of only the characters *not* found in SRC."
  (let ((charset (string->char-set src)))
    (string-index dest (lambda (c)
			 (char-set-contains? charset c)))))

;; 7.24.5.4 The strpbrk function
;; STRPBRK doesn't translate because it returns a pointer.  It scans
;; the string DEST for any character in SRC, and returns a pointer
;; to that character.  Consider using the following version which
;; instead returns the index to the first matching character.  This
;; is identical to STRCSPN, above.
(define (strpbrk-idx dest src)
  "Return the index of the first character in DEST that is present
in SRC."
  (let ((charset (string->char-set src)))
    (string-index dest (lambda (c)
			 (char-set-contains? charset c)))))

;; 7.24.5.5 The strrchr function
;; STRRCHR doesn't really translate bedcause it returns a pointer
;; to the last matching character.  A good approximation is
;; (string-index-right STR CHAR)
(define (strrchr-idx str c)
  "Locates the index to the last occurrenc of c in the string STR."
  (string-index-right str c))

;; 7.24.5.6 The strspn function
(define (strspn dest src)
  "Returns the length of the maximum intial segment (span) of DEST
that consists only of the characters that are found in SRC."
  (let ((charset (string->char-set src)))
    (string-index dest (lambda (c)
			 (not (char-set-contains? charset c))))))

;; 7.24.5.7 The strstr function 
;; STRSTR doesn't translate because it returns a pointer. It scans
;; DEST for an occurrence of the string SRC, returning a pointer
;; to the location of the beginning of the occurrence.  Consider
;; the following replacement
(define (strstr-idx dest src)
  "Return the index of the location where SRC appears in the string
DEST, or #f if the string STR can not be found in DEST."
  (string-contains dest src))

;; 7.24.5.8 The strtok function

;; 7.24.6.2 The memset function
;; MEMSET fills a raw location in memory.  If we're using bytevectors
;; to hold raw blocks, it could be implemented like the following.
(define (memset dest ch count)
  "Fill the first COUNT bytes of bytevector DEST with the unsigned
8-bit integer ch."
  (let ((end (min count (bytevector-length dest))))
    (if (= end (bytevector-length dest))
	(bytevector-fill! dest ch)
	;; else
	(do ((i 0 (1+ i)))
	    ((>= i end))
	  (bytevector-u8-set! dest i ch)))))

;; 7.24.6.2 The strerror function 
;; STRERROR returns a text version of errno.  It exists in core Guile.

;; 7.24.6.3 The strlen function
(define (strlen str)
  "Return the length of STR."
  (string-length str))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; <math.h>





(define (ilogb val)
  'fixme)

(define (ldexp val)
  'fixme)

;; With a real implementaion of log1p, it should be more accurate that
;; (log (1+ x))
(define (log1p x)
  "Compute the natural logarithm of val + 1."
  (log (1+ x)))

(define (log2 x)
  'fixme)

(define (logb x)
  'fixme)

(define (modf x)
  'fixme)

(define (scalbn x n)
  'fixme)

(define (cbrt x)
  'fixme)

(define (hypot x y)
  'fixme)

(define (pow x y)
  (expt x y))

(define (erf x)
  'fixme)

(define (lgamma x)
  'fixme)

(define (tgamma x)
  'fixme)

(define (ceil x)
  (ceiling x))

(define (lround x)
  "Rounds to the nearest integer value, but, for halfway cases,
always rounds away from zero."
  'fixme)

(define (trunc x)
  "Round positive numbers down toward zero.  Round negative numbers
up toward zero."
  (truncate x))

(define (fmod x y)
  ;; some other remainder function
  'fixme)

(define (remquo x y)
  'fixme)


(define (fdim x y)
  "if x > y, returns x - y.  Otherwise, returns zero."
  'fixme)

;; A proper implementation may avoid rounding error.
(define (fma x y z)
  
  (+ (* x y) z))


;; ABS - is in core Guile

;; DIV - returns a structure with quotient and remainder. One
;; could mock it up like this.

;; IMAXABS
;; IMAXDIV

;; FABS
;; FMOD
;; REMAINDER
;; REMQUO
;; FMA
;; FMAX
;; FMIN
;; FDIM
;; NAN
;; EXP
;; LOG
;; LOG10
;; LOG2
;; LOG1P
;; POW
;; SQRT
;; CBRT
;; HYPOT
;; SIN
;; COS
;; TAR
;; ASIN
;; ACOS
;; ATAN
;; ATAN2
;; SINH
;; COSH
;; TANH
;; ASINH
;; ACOSH
;; ATANH
;; ERF
;; ERFC
;; TGAMMA
;; LGAMMA
;; CEIL
;; FLOOR
;; TRUNC
;; ROUND
;; NEARBYINT
;; RINT
;; LRINT
;; LLRINT
;; FREXP
;; LDEXP
;; MODF
;; SCALBN
;; ILOGB
;; LOGB
;; NEXTAFTER
;; NEXTTOWARD

;; FPCLASSIFY
;; ISFINITE
;; ISINF
;; ISNAN
;; ISNORMAL
;; ISGREATER
;; ISGREATEREQUAL
;; ISLESS
;; ISLESSEQUAL
;; ISLESSGREATER
;; ISUNORDERED
;; HUGE_VALF
;; HUGE_VAL
;; HUGE_VALL
;; INFINITY
;; NAN
;; FP_FAST_FMAF
;; FP_FAST_FMA
;; FP_FAST_FMAL
;; FP_ILOGB0
;; FP_ILOGBNAN
;; MATH_ERRHANDLING
;; MATH_ERRNO
;; MATH_ERREXCEPT
;; FP_NORMAL
;; FP_SUBNORMAL
;; FP_ZERO
;; FP_INFINITE
;; FP_NAN

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; <setjmp.h>

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; <signal.h>

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; <stdalign.h>

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; <stdarg.h>

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; <stdatomic.h>

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; <stdbool.h>

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; <stddef.h>

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; <stdint.h>


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; <stdio.h>



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; <tgmath.h>

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; <threads.h>

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; <time.h>

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; <uchar.h>

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; <wchar.h>


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; <wctype.h>

;; The Guile analogs of ISWALNUM, ISWALPHA, ISWDIGIT, ISWXDIGIT,
;; ISWCNTRL, ISWGRAPH, ISWSPACE, ISWBLANK, ISWPRINT, and ISWPUNCT
;; would all be the same as ISALNUM, etc, because Guile doesn't
;; distinguish between 8-bit and 32-bit characters.

;; WCTYPE, and ISWCTYPE have the same functionality as core Guile's
;; char-general-category, except that core Guile uses a symbol for the
;; Unicode general category.  Rough analogs to the C routines would be
;; like the following.
(define (iswctype ch desc)
  "Return #t if the character CH is in the Unicode character category
describe by the integer DESC.  To get a value for DESC, use the
WCTYPE procedure."
  (case desc
    ((1)
     (isalnum? ch))
    ((2)
     (isalpha? ch))
    ((3)
     (isblank? ch))
    ((4)
     (iscntrl? ch))
    ((5)
     (isdigit? ch))
    ((6)
     (isgraph? ch))
    ((7)
     (islower? ch))
    ((8)
     (isprint? ch))
    ((9)
     (isspace? ch))
    ((10)
     (isupper? ch))
    ((11)
     (isxdigit? ch))))

(define (wctype str)
  "Returns an integer that identifies
the following categories.  STR should be one of the following strings:
'alnum', 'alpha', 'blank', 'cntrl', 'digit', 'graph', 'lower', 'print',
'space', 'upper', 'xdigit'."
  (cond 
    ((string=? str "alnum") 1)
    ((string=? str "alpha") 2)
    ((string=? str "blank") 3)
    ((string=? str "cntrl") 4)
    ((string=? str "digit") 5)
    ((string=? str "graph") 6)
    ((string=? str "lower") 7)
    ((string=? str "print") 8)
    ((string=? str "space") 9)
    ((string=? str "upper") 10)
    ((string=? str "xdigit") 11)))
