#lang pl 02
;;*******************************************************************************************
;;*                                 Assignmet 2                                             *
;;* Author: Alexey Titov                                                                    *
;;* Date: 05/2019                                                                           *
;;* Version: 1.2                                                                            *
;;*******************************************************************************************

;;---------------------------------------------------------------------------------Question 1. BNF (SE)--------------------------------------------------------------------------------------------
;; a)
#| BNF for the SE language:
       <SE>     ::= <SE_NUM>                                           ;; (1)
                   | <SE_STR>                                          ;; (2)
                   | #\<DIGIT>                                         ;; (3)

       <SE_STR> ::= "<D>"                                              ;; (4)
                   | ( string <D_CHAR> )                               ;; (5)
                   | ( string-insert <SE_STR> #\<DIGIT> <SE_NUM> )     ;; (6)
                   | ( number->string <SE_NUM> )                       ;; (7)
                   | ( string-append  <SE_LST>)                        ;; (8)

       <SE_NUM> ::= <D>                                                ;; (9)
                   | ( string-length  <SE_STR> )                       ;; (10)

       <SE_LST> ::= λ                                                  ;; (11)
                   | <SE_STR> <SE_LST>                                 ;; (12)

       <DIGIT>  ::= 0 | 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9              ;; (13)
      
       <D_CHAR> ::= λ                                                  ;; (11)
                   | #\<DIGIT> <D_CHAR>                                ;; (14)

       <D>      ::= λ                                                  ;; (11)
                   | <DIGIT><D>                                        ;; (15)
        
|#

;; b)
#| 3 different SE expressions:

1) ( string-append ( string #\1 #\2 #\4 ) "12" )
    <SE>                                                                       = (2) >
    <SE_STR>                                                                   = (8) >
    ( string-append <SE_LST> )	                                               = (12) >
    ( string-append <SE_STR> <SE_LST> )                                        = (4) >
    ( string-append <SE_STR> "<D>" )                                           = (15) >
    ( string-append <SE_STR> "<DIGIT><D>" )                                    = (15) >
    ( string-append <SE_STR> "<DIGIT><DIGIT><D>" )                             = (11) >
    ( string-append <SE_STR> "<DIGIT><DIGIT>" )                                = (13) >
    ( string-append <SE_STR> "<DIGIT>2" )                                      = (13) >
    ( string-append <SE_STR> "12" )                                            = (5) >
    ( string-append ( string <D_CHAR> ) "12" )	                               = (14) >
    ( string-append ( string #\<DIGIT> <D_CHAR> ) "12" )	               = (14) >
    ( string-append ( string #\<DIGIT> #\<DIGIT> <D_CHAR> ) "12" )	       = (14) >
    ( string-append ( string #\<DIGIT> #\<DIGIT> #\<DIGIT> <D_CHAR> ) "12" )   = (11) >
    ( string-append ( string #\<DIGIT> #\<DIGIT> #\<DIGIT> ) "12" )            = (13) >
    ( string-append ( string #\<DIGIT> #\<DIGIT> #\4 ) "12" )                  = (13) >
    ( string-append ( string #\<DIGIT> #\2 #4 ) "12" )                         = (13) >
    ( string-append ( string #\1 #\2 #\4 ) "12" )		          

2) ( number->string ( string-length "034" ) )
    <SE>                                                                = (2) >
    <SE_STR>                                                            = (7) >
    ( number->string <SE_NUM> )	                                        = (10) >
    ( number->string ( string-length  <SE_STR> ) )                      = (4) >
    ( number->string ( string-length  "<D>" ) )                         = (15) >
    ( number->string ( string-length  "<DIGIT><D>" ) ) 	                = (15) >
    ( number->string ( string-length  "<DIGIT><DIGIT><D>" ) )           = (15) >
    ( number->string ( string-length  "<DIGIT><DIGIT><DIGIT><D>" ) )    = (11) >
    ( number->string ( string-length  "<DIGIT><DIGIT><DIGIT>" ) )       = (13) >
    ( number->string ( string-length  "<DIGIT><DIGIT>4" ) ) 	        = (13) >
    ( number->string ( string-length  "<DIGIT>34" ) )	                = (13) >
    ( number->string ( string-length  "034" ) ) 	                  

3) ( number->string 123456 )
    <SE>                                                             = (2) >
    <SE_STR>                                                         = (6) >
    ( number->string <SE_NUM> )  	                             = (9) >
    ( number->string <D> )                                           = (15) >
    ( number->string <DIGIT><D> )                                    = (15) >
    ( number->string <DIGIT><DIGIT><D> ) 	                     = (15) >
    ( number->string <DIGIT><DIGIT><DIGIT><D> ) 	             = (15) >
    ( number->string <DIGIT><DIGIT><DIGIT><DIGIT><D> ) 	             = (15) >
    ( number->string <DIGIT><DIGIT><DIGIT><DIGIT><DIGIT><D> )        = (15) >
    ( number->string <DIGIT><DIGIT><DIGIT><DIGIT><DIGIT><DIGIT><D> ) = (11) >
    ( number->string <DIGIT><DIGIT><DIGIT><DIGIT><DIGIT><DIGIT> )    = (13) >
    ( number->string <DIGIT><DIGIT><DIGIT><DIGIT><DIGIT>6 )          = (13) >
    ( number->string <DIGIT><DIGIT><DIGIT><DIGIT>56 )                = (13)>
    ( number->string <DIGIT><DIGIT><DIGIT>456 )                      = (13) >
    ( number->string <DIGIT><DIGIT>3456 )                            = (13) >
    ( number->string <DIGIT>23456 )                                  = (13) >
    ( number->string 123456 )

|#

;;---------------------------------------------------------------------------------Question 2. Accommodating Memory Operations---------------------------------------------------------------------
;; 1)
#|

   {* {+ {set 1} {set 2}} get}
   These specifications suffer from being ambiguous: an expression can be derived in multiple ways (i.e., have multiple derivation trees).  
   We want to get rid of this ambiguity, so that there is a single (= deterministic) way to derive all expressions.
   So we define that working in the form Left association and get can not be the most left and appear after minimum set one.
|#

;; 2.1)
#|
   {* {set {+ 2 {/ 18 12}}} get}

   <MAE>  ::=  { seq <AE> }                                             ;; 1
             | { seq {set <AE>} {set <GMAE>} <GMAE> }                   ;; 2

   <AE>   ::= <num>                                                     ;; 3
             | { + <AE> <AE> }                                          ;; 4
             | { - <AE> <AE> }                                          ;; 5
             | { * <AE> <AE> }                                          ;; 6
             | { / <AE> <AE> }                                          ;; 7

   <GMAE> ::= get                                                       ;; 8
             | <AE>                                                     ;; 9
             | { + <GMAE> <GMAE> }                                      ;; 10
             | { - <GMAE> <GMAE> }                                      ;; 11
             | { * <GMAE> <GMAE> }                                      ;; 12
             | { / <GMAE> <GMAE> }                                      ;; 13

   <DIGIT> ::= 0 | 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9                    ;; 14

   <num>   ::= <DIGIT>                                                  ;; 15
              | <DIGIT><num>                                            ;; 16

|#

;; 2.2)
#| 3 different MAE expressions:

1){seq {set {+ 33 406}} {set {* get get}} {/ get 30}}
<MAE>                                                                                       = (2) >
    { seq {set <AE>} {set <GMAE>} <GMAE> }  	                                            = (4) >
    { seq {set { + <AE> <AE> }} {set <GMAE>} <GMAE> }                                       = (3) >
    { seq {set { + <num> <AE> }} {set <GMAE>} <GMAE> }                                      = (16) >
    { seq {set { + <DIGIT><num> <AE> }} {set <GMAE>} <GMAE> }                               = (14) >
    { seq {set { + 3<num> <AE> }} {set <GMAE>} <GMAE> }                                     = (15) >
    { seq {set { + 33 <AE> }} {set <GMAE>} <GMAE> }                                         = (15) >
    { seq {set { + 33 <AE> }} {set <GMAE>} <GMAE> }                                         = (3) >
    { seq {set { + 33 <num> }} {set <GMAE>} <GMAE> }                                        = (16) >
    { seq {set { + 33 <DIGIT><num> }} {set <GMAE>} <GMAE> }                                 = (15) >
    { seq {set { + 33 4<num> }} {set <GMAE>} <GMAE> }                                       = (16) >
    { seq {set { + 33 4<DIGIT><num> }} {set <GMAE>} <GMAE> }                                = (14) >
    { seq {set { + 33 40<num> }} {set <GMAE>} <GMAE> }                                      = (15) >
    { seq {set { + 33 40<DIGIT> }} {set <GMAE>} <GMAE> }                                    = (14) >
    { seq {set { + 33 406 }} {set <GMAE>} <GMAE> }                                          = (12) >
    { seq {set { + 33 406 }} {set { * <GMAE> <GMAE> }} <GMAE> } 	                    = (8) >
    { seq {set { + 33 406 }} {set { * get <GMAE> }} <GMAE> } 	                            = (8) >
    { seq {set { + 33 406 }} {set { * get get }} <GMAE> }                                   = (13) >
    { seq {set { + 33 406 }} {set { * get get }} { / <GMAE> <GMAE> } }                      = (8) >
    { seq {set { + 33 406 }} {set { * get get }} { / get <GMAE> } }                         = (3) >
    { seq {set { + 33 406 }} {set { * get get }} { / get <num> } }                          = (16) >
    { seq {set { + 33 406 }} {set { * get get }} { / get <DIGIT><num> } }                   = (14) >
    { seq {set { + 33 406 }} {set { * get get }} { / get 3<num> } }                         = (15) >
    { seq {set { + 33 406 }} {set { * get get }} { / get 3<DIGIT> } }                       = (14) >
    { seq {set { + 33 406 }} {set { * get get }} { / get 30 } }                       

2){seq {- 30 21}}
<MAE>                                                                                       = (1) >
    { seq <AE> }                                                                            = (5) >
    { seq { - <AE> <AE> } }                                                                 = (3) >
    { seq { - <num> <AE> } }                                                                = (16) >
    { seq { - <DIGIT><num> <AE> } }                                                         = (14) >
    { seq { - 3<num> <AE> } }                                                               = (15)> >
    { seq { - 3<DIGIT> <AE> } }                                                             = (14) >
    { seq { - 30 <AE> } }                                                                   = (16) >
    { seq { - 30 <DIGIT><num> } }                                                           = (14) >
    { seq { - 30 2<num> } }                                                                 = (15) >
    { seq { - 30 2<DIGIT> } }                                                               = (14) >
    { seq { - 30 21 } }

3){seq {set {+ 630 21}} {set {+ get get}} {* get get}}
<MAE>                                                                                       = (2) >
    { seq {set <AE>} {set <GMAE>} <GMAE> }  	                                            = (4) >
    { seq {set { + <AE> <AE> }} {set <GMAE>} <GMAE> }                                       = (3) >
    { seq {set { + <num> <AE> }} {set <GMAE>} <GMAE> }                                      = (16) >
    { seq {set { + <DIGIT><num> <AE> }} {set <GMAE>} <GMAE> }                               = (14) >
    { seq {set { + 6<num> <AE> }} {set <GMAE>} <GMAE> }                                     = (16) >
    { seq {set { + 6<DIGIT><num> <AE> }} {set <GMAE>} <GMAE> }                              = (14) >
    { seq {set { + 63<num> <AE> }} {set <GMAE>} <GMAE> }                                    = (15) >
    { seq {set { + 63<DIGIT> <AE> }} {set <GMAE>} <GMAE> }                                  = (14) >
    { seq {set { + 630 <AE> }} {set <GMAE>} <GMAE> }                                        = (3) >
    { seq {set { + 630 <num> }} {set <GMAE>} <GMAE> }                                       = (16) >
    { seq {set { + 630 <DIGIT><num> }} {set <GMAE>} <GMAE> }                                = (14) >
    { seq {set { + 630 2<num> }} {set <GMAE>} <GMAE> }                                      = (15) >
    { seq {set { + 630 2<DIGIT> }} {set <GMAE>} <GMAE> }                                    = (14) >
    { seq {set { + 630 21 }} {set <GMAE>} <GMAE> }                                          = (10) >
    { seq {set { + 630 21 }} {set { + <GMAE> <GMAE> }} <GMAE> } 	                    = (8) >
    { seq {set { + 630 21 }} {set { + get <GMAE> }} <GMAE> } 	                            = (8) >
    { seq {set { + 630 21 }} {set { + get get }} <GMAE> }                                   = (12) >
    { seq {set { + 630 21 }} {set { + get get }} { * <GMAE> <GMAE> } }                      = (8) >
    { seq {set { + 630 21 }} {set { + get get }} { * get <GMAE> } }                         = (8) >
    { seq {set { + 630 21 }} {set { + get get }} { * get get } }

|#

;;---------------------------------------------------------------------------------Question 3. Higher Order Functions------------------------------------------------------------------------------
#|
  The function takes a number as input, and produces a number which is a square of the number.
  x - number from list 
|#
(: square : Number -> Number)
(define (square x)
  (* x x))                                 ; x * x

#|
  The function takes a list of numbers as input, and produces a number which is the sum of the squares of all of the numbers in the list.
  lst - a list of numbers
|#
(: sum-of-squares :(Listof Number) -> Number)
(define (sum-of-squares lst)
  (foldl + 0 (map square lst)))            ; (map square lst) => (map (lambda (x) (* x x)) lst)
                                           ; (foldl + 0 newlst) => 0+item[0]+item[1]+.........

;; tests --- square/ sum-of-squares
(test (square -2) => 4)                           ; -2 * -2 = 4
(test (square 0) => 0)                            ; 0 * 0 = 0
(test (square 5) => 25)                           ; 5* 5 = 25
(test (map square '(1 2 3)) => '(1 4 9))          ; '(1*1 2*2 3*3) = '(1 4 9) 
(test (foldl + 0 '(1 2 3)) => 6)                  ; 0+1+2+3 = 6
(test (sum-of-squares '(1 2 3)) => 14)            ; 0+'(1*1 2*2 3*3) = 0+1+4+9 = 14

;;---------------------------------------------------------------------------------Question 4. PAE (and more H.O. functions)-----------------------------------------------------------------------
;; a)
#|
  The function takes as arguments a list of x numbers a0,...,ak−1 and returns as output a function.
  The returned function takes a number x0 and return the value of the polynomial a0*x0+...+ak−1*xn−1 at x0.
  coeffs - a list of coefficients a
|#
(: createPolynomial : (Listof Number) -> Number -> Number)
(define (createPolynomial coeffs)
  #|
    The function calculate the value of the polynomial a0*x0+...+ak−1*xn−1 at x0.
    argsL - a list of k numbers a0,...,ak-1
    x - a number Xo
    power - power of Xo
    accum - the value of the polynomial a0⋅x0+⋯+ak-1⋅xn-1 at x0
  |#
  (: poly : (Listof Number) Number Integer Number -> Number)
  (define (poly argsL x power accum)
    (if (null? argsL)                                                                           ;; call function null? to check for empty list 
        accum                                                                                   ;; the value of the polynomial a0*x0+...+ak-1*xn−1 at x0.
        (poly (rest argsL) x (+ power 1) (+ accum ( * (first argsL) (expt x power))))))         ;; move to the next element
  #|
    The function call to tail recursion the function poly
    x - a number Xo
  |#
  (: polyX : Number -> Number)
  (define (polyX x)
      (poly coeffs x 0 0))                                                                      ;; coeffs - the list of coefficients a; x - variable; 0 - at first power equals zero; 0 - at first accum equals zero
  polyX)                                                                                        ;; return the function polyX

;; tests --- createPolynomial
(define p2345 (createPolynomial '(2 3 4 5)))
(test (p2345 0) => (+ (* 2 (expt 0 0)) (* 3 (expt 0 1)) (* 4 (expt 0 2)) (* 5 (expt 0 3))))         ;; 2
(test (p2345 4) => (+ (* 2 (expt 4 0)) (* 3 (expt 4 1)) (* 4 (expt 4 2)) (* 5 (expt 4 3))))         ;; 398
(test (p2345 11) => (+ (* 2 (expt 11 0)) (* 3 (expt 11 1)) (* 4 (expt 11 2)) (* 5 (expt 11 3))))    ;; 7174

(define p536 (createPolynomial '(5 3 6)))
(test (p536 11) => (+ (* 5 (expt 11 0)) (* 3 (expt 11 1)) (* 6 (expt 11 2))))                       ;; 764

(define p_0 (createPolynomial '()))
(test (p_0 4) => 0)                                                                                 ;; 0

;; b.1)

#|
  The grammar:
              <PLANG> ::= { Poly <AEs> <AEs> }          ;; (Listof AE) (Listof AE)

              <AEs> ::= <AE>                            ;; (Listof AE)
                       | <AE> <AEs>                  

              <AE> ::= <num>                            ;; Num
                      | { + <AE> <AE> }                 ;; Add
                      | { - <AE> <AE> }                 ;; Sub
                      | { * <AE> <AE> }                 ;; Mul
                      | { / <AE> <AE> }                 ;; Div

|#

;; b.2)

;; Type definition of PLANG
(define-type PLANG
  [Poly (Listof AE) (Listof AE)])

;; Type definition of AE
(define-type AE
  [Num Number]
  [Add AE AE]
  [Sub AE AE]
  [Mul AE AE]
  [Div AE AE])

(: parse-sexpr : Sexpr -> AE)
;; to convert s-expressions into AEs
(define (parse-sexpr sexpr)
  (match sexpr
    [(number: n) (Num n)]                                        ;; <num>
    [(list '+ lhs rhs) (Add (parse-sexpr lhs)                    ;; { + <AE> <AE> }
                            (parse-sexpr rhs))]
    [(list '- lhs rhs) (Sub (parse-sexpr lhs)                    ;; { - <AE> <AE> }
                            (parse-sexpr rhs))]
    [(list '* lhs rhs) (Mul (parse-sexpr lhs)                    ;; { * <AE> <AE> }
                            (parse-sexpr rhs))]
    [(list '/ lhs rhs) (Div (parse-sexpr lhs)                    ;; { / <AE> <AE> }
                            (parse-sexpr rhs))]
    [else (error 'parse-sexpr "bad syntax in ~s"                 ;; error
                 sexpr)]))

#|
  The function helps to the function parse to parse and returns a list of AE.
  lst - a list of AE
  Sexpr - s-expression
|#
#|
(: help-parser : (Listof AE) Sexpr -> (Listof AE))
(define (help-parser lst sexpr)
  (if (null? sexpr)                                                                      ;; call function null? to check for s-expression
      lst                                                                                ;; return list
      (match sexpr
        [(list n ) (append lst (list (parse-sexpr n)))]                                  ;; one item
        [(list a b ...) (help-parser (append lst (list (parse-sexpr a))) b)]             ;; several items
        [else (error 'help-parser "bad syntax in ~s" sexpr)])))                          ;; error
|#

(: parse : String -> PLANG)
;; parses a string containing a PLANG expression to a PLANG AST
(define (parse str)
  (let ([code (string->sexpr str)])                                                    ;; convert string to s-expression and assignment of variable code value
    (match code
      [(list first second) (Poly                                                                                              ;; initialization of variable Poly
                            (match first                                                                                      ;; first list of AE
;;                            [(list 'poly a b ...) (help-parser (list (parse-sexpr a)) b )]                                  ;; check that there is a symbol poly and at least one number
                              [(list 'poly a b ...) (map parse-sexpr (rest first))]                                           ;; with map
                              [(list 'poly) (error 'parse "at least one coefficient is required in (~s ~s)" first second)]    ;; error
                              [else (error 'parse "bad 'poly' syntax in ~s" code)])                                           ;; 'poly symbol missing
                            (match second                                                                                     ;; second list of AE
;;                            [(list a b ...) (help-parser (list (parse-sexpr a)) b )]                                        ;; check that at least one number
                              [(list a b ...) (map parse-sexpr second)]                                                       ;; with map
                              [else (error 'parse "at least one point is required in (~s ~s)" first second)]))]               ;; error
      [(list) (error 'parse "bad syntax in ~s" code)]                                                                         ;; an empty list
      [else (error 'parse "bad 'poly' syntax in ~s" code)])))                                                                 ;; error

;; test -- parse/Poly
;;(test (help-parser (list (Num 1)) (string->sexpr "{}")) => (list (Num 1)))                                         ;; '(1) '() => '(2)
;;(test (help-parser '() (string->sexpr "{1 2 3}")) => (list (Num 1) (Num 2) (Num 3)))                               ;; '() '(1 2 3) => '(1 2 3)
;;(test (help-parser (list (Num 1)) (string->sexpr "{poly}"))  =error> "parse-sexpr: bad syntax in poly")
(test (parse "{{poly 1 2 3} {1 2 3}}") => (Poly (list (Num 1) (Num 2) (Num 3)) (list (Num 1) (Num 2) (Num 3))))    ;; {{poly 1 2 3} {1 2 3}} => (Poly (List (Num 1) (Num 2) (Num 3)) (List (Num 1) (Num 2) (Num 3)))
(test (parse "{{poly } {1 2} }") =error> "parse: at least one coefficient is required in ((poly) (1 2))")
(test (parse "{{poly 1 2} {} }") =error> "parse: at least one point is required in ((poly 1 2) ())")
(test (parse "{}") =error> "parse: bad syntax in ()")
;; from forum
(test (parse "{{+ 1 2} { 2} }") =error> "parse: bad 'poly' syntax in ((+ 1 2) (2))")
(test (parse "{+ 1 2}") =error> "parse: bad 'poly' syntax in (+ 1 2)")


;; b.3)

(: eval : AE -> Number)
;; evaluates AE expressions to numbers
(define (eval expr)
(cases expr
  [(Num n) n]
  [(Add l r) (+ (eval l) (eval r))]
  [(Sub l r) (- (eval l) (eval r))]
  [(Mul l r) (* (eval l) (eval r))]
  [(Div l r) (/ (eval l) (eval r))]))

#|
  The function calculates polinomials and return answer list.
  ans - a list of answers
  poly - polinomial (polyX)
  coeff - alist of coefficients
|#
#|
(: cal-eval-poly : (Listof Number) (Number -> Number) (Listof Number) -> (Listof Number))
(define (cal-eval-poly ans poly coeff)
  (if (null? coeff)                                                                    ;; call function null? to check for empty list
      ans                                                                              ;; return answer
      (cal-eval-poly (append ans (list (poly (first coeff)))) poly (rest coeff))))     ;; move next
|#

#|
  The function calls function cal-eval-poly, which calculates polinomials and return answer list.
  p-expr - Poly with polinomial and the list of coefficients
|#
(: eval-poly : PLANG -> (Listof Number))
(define (eval-poly p-expr)
  (cases p-expr
;;  [(Poly first second) (cal-eval-poly '() (createPolynomial (map eval first)) (map eval second))]
    [(Poly first second) (map (createPolynomial (map eval first)) (map eval second))]))                        ;; with map

(: run : String -> (Listof Number))
;; evaluate a PLANG program contained in a string
(define (run str)
  (eval-poly (parse str)))


;; tests run/eval/eval-poly/cal-eval-poly
;;(define p (createPolynomial (map eval (list (Num 1) (Num 2) (Num 3)))))
;;(test (cal-eval-poly '() p (map eval (list (Num 1)))) => '(6))                    ;; 1*1+2*1+3*1 = 6
;;(test (cal-eval-poly '() p '()) => '())                                           ;; () = ()

(test (run "{{poly 1 2 3} {1 2 3}}") => '(6 17 34))                               ;; ( (1*1+2*1+3*1 = 6) (1*1+2*2+3*4 = 17) (1*1+2*3+3*9 = 34) )
(test (run "{{poly 4 2 7} {1 4 9}}") => '(13 124 589))                            ;; ( (4*1+2*1+7*1 = 13) (4*1+2*4+7*16 = 124) (4*1+2*9+7*81 = 589) )
(test (run "{{poly 1 2 3} {1 2 3}}") => '(6 17 34))                               ;; ( (1*1+2*1+3*1 = 6) (1*1+2*2+3*4 = 17) (1*1+2*3+3*9 = 34) )
(test (run "{{poly 4/5 } {1/2 2/3 3}}") => '(4/5 4/5 4/5))                        ;; ( (4/5*1 = 4/5) (4/5*1 = 4/5) (4/5*1 = 4/5) )
(test (run "{{poly 2 3} {4}}") => '(14))                                          ;; (2*1+3*4 = 14)
(test (run "{{poly 1 1 0} {-1 3 3}}") => '(0 4 4))                                ;; ( (1*1+1*-1+0*1 = 0) (1*1+1*3+0*9 = 4) (1*1+1*3+0*9 = 4) )
;; from forum
(test (run "{{poly {- 2 1} { * 1 1} {* 0 {+ 15 2}}} {{- 1 2} 3 3}}") => '(0 4 4))