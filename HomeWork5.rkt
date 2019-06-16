#lang pl
;;*******************************************************************************************
;;*                                 Assignmet 5                                             *
;;* Author: Alexey Titov                                                                    *
;;* Date: 06/2019                                                                           *
;;* Version: 1.0                                                                            *
;;*******************************************************************************************

;;---------------------------------------------------------------------------------Question 1. Re-Implementing the ROL language in the Environment Model--------------------------------------------------------------------------------------------
;;                                                                                    AND
;;---------------------------------------------------------------------------------Question 2. "with" as a Syntactic Sugar--------------------------------------------------------------------------------------------

#| BNF for the ROL language:

   <ROL> ::= {reg-len= <num> <RegE>}

   <RegE> ::= <Bits>
             |{and <RegE> <RegE>} 
             |{or <RegE> <RegE>} 
             |{shl <RegE>} 
             |{WITH {<ID> <RegE> }<RegE>}
             |{<ID>}
             |{geq? <RegE> <RegE>}
             |{maj? <RegE>}
             |{if <RegE> <RegE> <RegE>}
             |{ FUN { <ID> } <RegE> }                                                       ;; NEW
             |{ CALL <RegE> <RegE> }                                                        ;; NEW
             | <Bool>


   <Bits> ::= <BIT>
             | <BIT><Bits>

   <BIT> ::= 1 | 0

   <DIGIT> ::= 0 | 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9                    

   <num>   ::= <DIGIT>                                                  
              | <DIGIT><num>                                            

|#

;; Defining two new types
(define-type BIT = (U 0 1))
(define-type Bit-List = (Listof BIT))

;; RegE abstract syntax trees
(define-type RegE
  [Reg  Bit-List]
  [And RegE RegE]
  [Or RegE RegE]
  [Shl RegE]
  [ID Symbol]
  [WITH Symbol RegE RegE]
  [Bool Boolean]
  [Geq RegE RegE]
  [Maj RegE]
  [If RegE RegE RegE]
  [FUN  Symbol RegE]                                                                       ;; NEW
  [CALL RegE RegE])                                                                        ;; NEW


;; Next is a technical function that converts (casts)
;; (any) list into a bit-list. We use it in parse-sexpr.
(: list->bit-list : (Listof Any) -> Bit-List)
;; to cast a list of bits as a bit-list
(define (list->bit-list lst)
  (cond [(null? lst) null]                                                       ;; call function null? to check for empty list
        [(eq? (first lst) 1)(cons 1 (list->bit-list (rest lst)))]                ;; number 1
        [else (cons 0 (list->bit-list (rest lst)))]))                            ;; number 0

(: parse-sexprFROL : Sexpr -> RegE)
;; to convert the main s-expression into ROL
(define (parse-sexprFROL sexpr)
  (match sexpr
    [(list 'reg-len '= (number: n) args)
     (if (> n 0)                                                                  ;; remember to make sure specified register length is at least 1
         (parse-sexpr-RegL args n)
         (error 'parse-sexprFROL "Register length must be at least 1 ~s" sexpr))]     ;; reg-len > 0
    [else (error 'parse-sexprFROL "bad syntax in ~s" sexpr)]))                        ;; error of syntax

(: parse-sexpr-RegL : Sexpr Number -> RegE)
;; to convert s-expressions into RegEs
(define (parse-sexpr-RegL sexpr reg-len)
  (match sexpr
    [(list (and a (or 1 0)) ... ) (if (= reg-len (length a))
                                      (Reg (list->bit-list a))                                                                                    ;; Reg
                                      (error 'parse-sexprFROL "wrong number of bits in ~s" a))]                                                   ;; error of Reg
    [(list 'and list1 list2) (And (parse-sexpr-RegL list1 reg-len) (parse-sexpr-RegL list2 reg-len))]                                             ;; And
    [(list 'or list1 list2) (Or (parse-sexpr-RegL list1 reg-len) (parse-sexpr-RegL list2 reg-len))]                                               ;; Or
    [(list 'shl list) (Shl (parse-sexpr-RegL list reg-len))]                                                                                      ;; Shl
    [(boolean: bool-value) (Bool bool-value)]                                                                                                     ;; Bool, don't work because string->sexpr: syntax error (bad contents)
    [(symbol: id-name) (cond [(eq? id-name 'false) (Bool false)]                                                                                  ;; Bool false
                             [(eq? id-name 'true) (Bool true)]                                                                                    ;; Bool true
                             [else (ID id-name)])]                                                                                                ;; Id
    [(cons 'with args)             
     (match sexpr
       [(list 'with (list(symbol: oldName) newName) body)                                                                                         ;; With 
        (WITH oldName (parse-sexpr-RegL newName reg-len) (parse-sexpr-RegL body reg-len))]                  
       [else (error 'parser-sexpr-RegL "bad `with' syntax in ~s" sexpr)])]                                                                        ;; error of With 
    [(list 'geq? list1 list2) (Geq(parse-sexpr-RegL list1 reg-len) (parse-sexpr-RegL list2 reg-len))]                                             ;; Geq
    [(list 'maj? list) (Maj (parse-sexpr-RegL list reg-len))]                                                                                     ;; Maj
    [(list 'if list1 list2 list3) (If (parse-sexpr-RegL list1 reg-len) (parse-sexpr-RegL list2 reg-len) (parse-sexpr-RegL list3 reg-len))]        ;; If
    [(cons 'fun more)                                                                                                                             ;; NEW, Fun
     (match sexpr
       [(list 'fun (list (symbol: name)) body)
        (FUN name (parse-sexpr-RegL body reg-len))]
       [else (error 'parse-sexpr-RegL "bad `fun' syntax in ~s" sexpr)])]                                                                          ;; error of parse-sexpr-RegL
    [(list 'call fun arg) (CALL (parse-sexpr-RegL fun reg-len) (parse-sexpr-RegL arg reg-len))]                                                   ;; NEW, Call
    [else (error 'parse-sexprFROL "bad syntax in ~s" sexpr)]))                                                                                    ;; error of parse-sexpr-RegL

(: parseFROL : String -> RegE)
;; parses a string containing a RegE expression to a RegE AST
(define (parseFROL str)
  (parse-sexprFROL (string->sexpr str)))


;; Types for environments, values, and a lookup function
(define-type ENV
  [EmptyEnv]
  [Extend Symbol RES ENV])

(define-type RES
  [RegV Bit-List]                       ;; register output
  [RegB Boolean]                        ;; boolean output
  [RegF Symbol RegE ENV])               ;; fun output

;; pop variables RES from the environment model  
(: lookup : Symbol ENV -> RES)
(define (lookup name env)
  (cases env
    [(EmptyEnv) (error 'lookup "free identifier: ~s" name)]            ;; a variable is not found     ;;(error 'lookup "no binding for ~s" name)
    [(Extend id val rest-env)
     (if (eq? id name) val (lookup name rest-env))]))                  ;; true - return val           false - next


#| Formal specs for `eval':
eval(Reg) = Reg
eval(bl) = bl
eval(true) = true
eval(false) = false
eval({and E1 E2}) = (<x1 bit-and y1> <x2 bit-and y2> ... <xk bit-and yk>), where eval(E1) = (x1 x2 ... xk) and eval(E2) = (y1 y2 ... yk)
eval({or E1 E2}) = (<x1 bit-or y1> <x2 bit-or y2> ... <xk bit-or yk>,) where eval(E1) = (x1 x2 ... xk) and eval(E2) = (y1 y2 ... yk)
eval({shl E}) = (x2 ... xk x1), where eval(E) = (x1 x2 ... xk)
eval({with {x E1} E2}) = eval(E2[eval(E1)/x])
eval({if E1 E2 E3}) = eval(E3) if eval(E1) = false = eval(E2) otherwise
eval({maj? E}) = true if x1+x2+...+xk >= k/2, and false otherwise, where eval(E) = (x1 x2 ... xk)
eval({geq? E1 E2}) = true if x_i >= y_i, where eval(E1) = (x1 x2 ... xk) and eval(E2) = (y1 y2 ... yk) and i is the first index s.t. x_i and y_i are not equal (or i =k if all are equal)
eval({if Econd Edo Eelse}) = eval(Edo) if eval(Econd) =/= false, = eval(Eelse), otherwise.

eval(FUN) = FUN ; assuming FUN is a function expression
eval({call E1 E2}) = eval(Ef[eval(E2)/x])   if eval(E1) = {fun {x} Ef}
                   = error!                 otherwise
|#

;;-------------------------------------- the evaluation part for the substitution model------------------------------
(: evalFROL : RegE ENV -> RES)
;; evaluates RegE expressions by reducing them to bit-lists
(define (evalFROL expr env)
(cases expr
  [(Reg BL) (RegV BL)]                                                                                                         ;; eval(Reg) = Reg
  [(Bool BOOL) (RegB BOOL)]                                                                                                    ;; eval(bl) = bl (eval(true) = true / eval(false) = false)
  [(Shl list) (RegV (shift-left (RegV->bit-list (evalFROL list env))))]                                                        ;; eval({shl E}) = (x2 ... xk x1), where eval(E) = (x1 x2 ... xk)
  [(Maj list) (RegB (majority? (RegV->bit-list (evalFROL list env))))]                                                         ;; eval({maj? E}) = true if x1+x2+...+xk >= k/2, and false otherwise, where eval(E) = (x1 x2 ... xk)
  [(Geq list1 list2) (RegB (geq-bitlists? (RegV->bit-list (evalFROL list1 env)) (RegV->bit-list (evalFROL list2 env))))]       ;; eval({geq? E1 E2}) = true if x_i >= y_i, where eval(E1) = (x1 x2 ... xk) and eval(E2) = (y1 y2 ... yk) and i is the first index s.t. x_i and y_i are not equal (or i =k if all are equal)
  [(And list1 list2)(reg-arith-op bit-and (evalFROL list1 env) (evalFROL list2 env))]                                          ;; eval({and E1 E2}) = (<x1 bit-and y1> <x2 bit-and y2> ... <xk bit-and yk>), where eval(E1) = (x1 x2 ... xk) and eval(E2) = (y1 y2 ... yk)
  [(Or list1 list2)(reg-arith-op bit-or (evalFROL list1 env) (evalFROL list2 env))]                                            ;; eval({or E1 E2}) = (<x1 bit-or y1> <x2 bit-or y2> ... <xk bit-or yk>,) where eval(E1) = (x1 x2 ... xk) and eval(E2) = (y1 y2 ... yk)
  [(FUN bound-id bound-body) (RegF bound-id bound-body env)]                                                                   ;; eval(FUN) = FUN ; assuming FUN is a function expression     
  [(CALL fun-expr arg-expr)
   (let ([fval (evalFROL fun-expr env)])
     (cases fval
       [(RegF bound-id bound-body f-env)                                                                                       ;; eval({call E1 E2}) = eval(Ef[eval(E2)/x])   if eval(E1) = {fun {x} Ef}
        (evalFROL bound-body
                  (Extend bound-id (evalFROL arg-expr env) f-env))]
       [else (error 'evalFROL "`call' expects a function, got: ~s" fval)]))]                                                   ;; error!                 otherwise
  [(If l c r) (if (RegB->boolean (evalFROL l env))                                                                             ;; l - eval(true) = true / eval(false) = false
                  (evalFROL c env)                                                                                             ;; eval({if Econd Edo Eelse}) = eval(Edo) if eval(Econd) =/= false, = eval(Eelse), otherwise.
                  (evalFROL r env))]
  [(ID name) (lookup name env)]                                                                                                ;; symbol
  [(WITH bound-id named-expr bound-body) (evalFROL (CALL (FUN bound-id bound-body) named-expr) env)]))                         ;; "with" as a Syntactic Sugar

;; Defining functions for dealing with arithmetic operations
;; on the above types
(: bit-and : BIT BIT -> BIT) ;; Arithmetic and
(define(bit-and a b)
  (if (= (+ a b) 2)         ;; Why (and a b) is not work?
      1                     ;; 1 and 1
      0))                   ;; 1 and 0        /   0 and 1        /   0 and 0

(: bit-or : BIT BIT -> BIT) ;; Aithmetic or
(define(bit-or a b)
  (if (>= (+ a b) 1)        ;; Why (or a b) is not work?
      1                     ;; 1 and 1        /   1 and 0        /   0 and 1 
      0))                   ;; 0 and 0


(: reg-arith-op : (BIT BIT -> BIT) RES RES -> RES)
;; Consumes two registers and some binary bit operation 'op',
;; and returns the register obtained by applying op on the
;; i'th bit of both registers for all i.
(define(reg-arith-op op reg1 reg2)
  (: bit-arith-op : Bit-List Bit-List -> Bit-List)
  ;; Consumes two bit-lists and uses the binary bit operation 'op'.
  ;; It returns the bit-list obtained by applying op on the
  ;; i'th bit of both registers for all i.
  (define(bit-arith-op bl1 bl2)
    (map op bl1 bl2))                                                                 ;; map - and / or between two bit-lists
  (RegV (bit-arith-op (RegV->bit-list reg1) (RegV->bit-list reg2))))                  ;; return register value

(: majority? : Bit-List -> Boolean)
;; Consumes a list of bits and checks whether the
;; number of 1's are at least as the number of 0's.
(define(majority? bl)
  (>= (foldl + 0 bl) (/ (length bl) 2)))                                              ;; if ((the number of 1's)>=(the number of 0's))  equal to if ((the number of 1's)>=(lenght/2))

(: geq-bitlists? : Bit-List Bit-List -> Boolean)
;; Consumes two bit-lists and compares them. It returns true if the
;; first bit-list is larger or equal to the second.
(define (geq-bitlists? bl1 bl2)
  (cond [(null? bl1) true]                                                            ;; call function null? to check for empty list, return true, the first bit-list is larger or equal to the second
        [(< (first bl1) (first bl2)) false]                                           ;; if the bit from the second bit-list is large to the bit from the first bit-list, return false, the second bit-list is larger to the first 
        [else (geq-bitlists? (rest bl1) (rest bl2))]))                                ;; move next

(: shift-left : Bit-List -> Bit-List)
;; Shifts left a list of bits (once)
;; https://stackoverflow.com/questions/13046017/rotate-a-list-to-the-left-in-scheme-racket
(define(shift-left bl)
  (append (cdr bl) (cons (car bl) '())))                                                       ;; to replace the first cons with an append for adding the first element at the end of the list


(: RegV->bit-list : RES -> Bit-List)
;; extract a bit-list from RES type
(define (RegV->bit-list expr)
  (cases expr
    [(RegV list) list]                                                                        ;; Bit-List
    [else (error 'RegV->bit-lis "boolean value instead of Bit-List in ~s" expr)]))            ;; boolean, error

(: RegB->boolean : RES -> Boolean)
;; extract a boolean from RES type
(define (RegB->boolean expr)
  (cases expr
    [(RegB BOOL) BOOL]                                                                        ;; boolean
    [else (error 'RegB->boolean "Bit-List instead of boolean in ~s" expr)]))                  ;; Bit-List, error 


;; new name because next questions
(: run : String -> Bit-List)
;; evaluate a ROL program contained in a string
;; we will not allow to return a boolean type
(define (run str)
  (let ([result (evalFROL (parseFROL str) (EmptyEnv))])
    (cases result
      [(RegV bl) bl]                                                                                ;; return Bit-List
      [else (error 'run "evaluation returned a bit-list: ~s" result)])))                            ;; error, we will not allow to return a boolean type

;; test bit-or / bit-and
(test (bit-or 0 1) => 1)
(test (bit-or 1 0) => 1)
(test (bit-or 1 1) => 1)
(test (bit-or 0 0) => 0)
(test (bit-and 0 1) => 0)
(test (bit-and 1 0) => 0)
(test (bit-and 1 1) => 1)
(test (bit-and 0 0) => 0)

;; test shl
(test (shift-left '(0 0 1 1)) => '(0 1 1 0))
(test (shift-left '(1 0 0 0)) => '(0 0 0 1))

;; test Geq
(test (geq-bitlists? '(1 0 1) '(1 1 1)) => #f)
(test (geq-bitlists? '(1 1) '(1 1)) => #t)
(test (geq-bitlists? '(1 1) '(1 0)) => #t)

;; test Maj
(test (majority? '(0 0)) => #f)
(test (majority? '(1 1)) => #t)
(test (majority? '(1 1 0 0)) => #t)
(test (majority? '(1 1 0 0 0)) => #f)

;; test RegV->bit-list
(test (RegV->bit-list (RegV (list 1 1 1 0))) => '(1 1 1 0))
(test (RegV->bit-list (RegB #t)) =error> "boolean value instead of Bit-List in (RegB #t)")

;; test RegB->boolean
(test (RegB->boolean (RegB #t)) => #t)
(test (RegB->boolean (RegV (list 1 1 1 0))) =error> "Bit-List instead of boolean in (RegV (1 1 1 0))")


;; tests from homework 3
(test (run "{ reg-len = 4 {1 0 0 0}}") => '(1 0 0 0))
(test (run "{ reg-len = 4 {shl {1 0 0 0}}}") => '(0 0 0 1))
(test (run "{ reg-len = 4 {and {shl {1 0 1 0}}{shl {1 0 1 0}}}}") => '(0 1 0 1))

;; tests from homework 3
(test (run "{ reg-len = 4 {1 0 0 0}}") => '(1 0 0 0))
(test (run "{ reg-len = 4 {shl {1 0 0 0}}}") => '(0 0 0 1))
(test (run "{ reg-len = 4 {and {shl {1 0 1 0}}{shl {1 0 1 0}}}}") => '(0 1 0 1))
(test (run "{ reg-len = 4 {or {and {shl {1 0 1 0}} {shl {1 0 0 1}}} {1 0 1 0}}}") => '(1 0 1 1))
(test (run "{ reg-len = 2 {or {and {shl {1 0}} {1 0}} {1 0}}}") => '(1 0))
(test (run "{ reg-len = 4 {with {x {1 1 1 1}} {shl y}}}") =error> "free identifier: y")
(test (run "{ reg-len = 2 {with {x { or {and {shl {1 0}} {1 0}} {1 0}}} {shl x}}}") => '(0 1))
(test (run "{ reg-len = 4 {or {1 1 1 1} {0 1 1}}}") =error> "wrong number of bits in (0 1 1)")
(test (run "{ reg-len = 0 {}}") =error> "Register length must be at least 1")
(test (run "{ reg-len = 3 {if {geq? {1 0 1} {1 1 1}} {0 0 1} {1 1 0}}}") => '(1 1 0))
(test (run "{ reg-len = 4 {if {maj? {0 0 1 1}} {shl {1 0 1 1}} {1 1 0 1}}}") => '(0 1 1 1))
(test (run "{ reg-len = 4 {if false {shl {1 0 1 1}} {1 1 0 1}}}") => '(1 1 0 1))
(test (run "{ reg-len = 2 {with {x false} {if x {1 1} {0 1}}}}") => '(0 1))


;; tests from homework 4
(test (run "{ reg-len = 3 {with {identity {fun {x} x}} {with {foo {fun {x} {or x {1 1 0}}}} {call {call identity foo} {0 1 0}}}}}") => '(1 1 0))
(test (run "{ reg-len = 3 {with {x {0 0 1}} {with {f {fun {y} {and x y}}} {with {x {0 0 0}} {call f {1 1 1}}}}}}") => '(0 0 1))
(test (run "{ reg-len = 4 {with {foo {fun {z} {if {maj? z} z {shl z}}}} {call foo {if {maj? {0 0 1 1}} {shl {1 0 1 1}} {1 1 0 1}}}}}") => '(0 1 1 1))

;; tests from homework 5
(test (run "{ reg-len = 3
{with {identity {fun {x} x}}
{with {foo {fun {x} {or x {1 1 0}}}}
{call {call identity foo} {0 1 0}}}}}")
      => '(1 1 0))
(test (run "{ reg-len = 3
{with {x {0 0 1}}
{with {f {fun {y} {and x y}}}
{with {x {0 0 0}}
{call f {1 1 1}}}}}}")
      => '(0 0 1))
(test (run "{ reg-len = 4
{with {foo {fun {z} {if {maj? z} z {shl z}}}}
{call foo {if {maj? {0 0 1 1}} {shl {1 0 1 1}} {1 1 0 1}}}}}")
      => '(0 1 1 1))