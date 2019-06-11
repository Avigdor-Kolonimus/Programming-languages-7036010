#lang pl
;;*******************************************************************************************
;;*                                 Assignmet 4                                             *
;;* Author: Alexey Titov                                                                    *
;;* Date: 05/2019                                                                           *
;;* Version: 1.0                                                                            *
;;*******************************************************************************************

;;---------------------------------------------------------------------------------Question 1. Adding function expressions to the ROL language--------------------------------------------------------------------------------------------

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


(define-type RES
  [RegV Bit-List]                       ;; register output
  [RegF Symbol RegE]                    ;; fun output
  [RegB Boolean])                       ;; boolean output

#| Formal specs for `subst':
(`BL' is a Bit-List, `E1', `E2' are <RegE>s, `x' is some <id>, `y' is a *different* <id>, ‘BOOL’ is Boolean )
BL[v/x] = BL
BOOL[v/x] = BOOL
{and E1 E2}[v/x] = {and E1[v/x] E2[v/x]}
{or E1 E2}[v/x] = {or E1[v/x] E2[v/x]}
{shl E}[v/x] = {shl E[v/x]}
y[v/x] = y
x[v/x] = v
{with {y E1} E2}[v/x] = {with {y E1[v/x]} E2[v/x]}
{with {x E1} E2}[v/x] = {with {x E1[v/x]} E2}
{if {E1} E2 E3}[v/x] = {if {E1[v/x]} E2[v/x] E3[v/x]}
{maj? E1}[v/x] = {maj? E1[v/x] }
{geq? E1}[v/x] = {geq? E1[v/x] }

{call E1 E2}[v/x] = {call E1[v/x] E2[v/x]}
{fun {y} E}[v/x] = {fun {y} E[v/x]}           ; if y =/= x
{fun {x} E}[v/x] = {fun {x} E}
|#

(: substFROL : RegE Symbol RegE -> RegE)
;; substitutes the second argument with the third argument in the
;; first argument, as per the rules of substitution; the resulting
;; expression contains no free instances of the second argument
(define (substFROL expr from to)
  (cases expr
    [(Reg BL) expr]                                                                     ;; BL[v/x] = BL
    [(Bool BOOL) expr]                                                                  ;; BOOL[v/x] = BOOL
    [(And l r) (And (substFROL l from to) (substFROL r from to))]                       ;; {and E1 E2}[v/x] = {and E1[v/x] E2[v/x]}
    [(Or l r) (Or (substFROL l from to) (substFROL r from to))]                         ;; {or E1 E2}[v/x] = {or E1[v/x] E2[v/x]}
    [(Geq l r) (Geq (substFROL l from to) (substFROL r from to))]                       ;; {geq? E1 E2}[v/x] = {geq? E1[v/x] E2[v/x]}
    [(Shl l) (Shl (substFROL l from to))]                                               ;; {shl E}[v/x] = {shl E[v/x]}
    [(Maj l) (Maj (substFROL l from to))]                                               ;; {maj? E1}[v/x] = {maj? E1[v/x] }
    [(If l c r) (If (substFROL l from to) (substFROL c from to) (substFROL r from to))]     ;; {if {E1} E2 E3}[v/x] = {if {E1[v/x]} E2[v/x] E3[v/x]}
    [(ID name) (if (eq? name from) to expr)]                                            ;; y[v/x] = y       ||  x[v/x] = v
    [(CALL l r) (CALL (substFROL l from to) (substFROL r from to))]                     ;; {call E1 E2}[v/x] = {call E1[v/x] E2[v/x]}
    [(FUN bound-id bound-body)
     (if (eq? bound-id from)
         expr                                                                           ;; {fun {x} E}[v/x] = {fun {x} E}
         (FUN bound-id (substFROL bound-body from to)))]                                ;; {fun {y} E}[v/x] = {fun {y} E[v/x]}           ; if y =/= x
    [(WITH bound-id named-expr bound-body)
     (WITH bound-id
           (substFROL named-expr from to)
           (if (eq? bound-id from)
               bound-body                                                              ;; {with {x E1} E2}[v/x] = {with {x E1[v/x]} E2}
               (substFROL bound-body from to)))]))                                     ;; {with {y E1} E2}[v/x] = {with {y E1[v/x]} E2[v/x]}


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
(: evalFROL : RegE -> RES)
;; evaluates RegE expressions by reducing them to bit-lists
(define (evalFROL expr)
(cases expr
  [(Reg BL) (RegV BL)]                                                                                                         ;; eval(Reg) = Reg
  [(Bool BOOL) (RegB BOOL)]                                                                                                    ;; eval(bl) = bl (eval(true) = true / eval(false) = false)
  [(Shl list) (RegV (shift-left (RegV->bit-list (evalFROL list))))]                                                            ;; eval({shl E}) = (x2 ... xk x1), where eval(E) = (x1 x2 ... xk)
  [(Maj list) (RegB (majority? (RegV->bit-list (evalFROL list))))]                                                             ;; eval({maj? E}) = true if x1+x2+...+xk >= k/2, and false otherwise, where eval(E) = (x1 x2 ... xk)
  [(Geq list1 list2) (RegB (geq-bitlists? (RegV->bit-list (evalFROL list1)) (RegV->bit-list (evalFROL list2))))]               ;; eval({geq? E1 E2}) = true if x_i >= y_i, where eval(E1) = (x1 x2 ... xk) and eval(E2) = (y1 y2 ... yk) and i is the first index s.t. x_i and y_i are not equal (or i =k if all are equal)
  [(And list1 list2)(reg-arith-op bit-and (evalFROL list1) (evalFROL list2))]                                                  ;; eval({and E1 E2}) = (<x1 bit-and y1> <x2 bit-and y2> ... <xk bit-and yk>), where eval(E1) = (x1 x2 ... xk) and eval(E2) = (y1 y2 ... yk)
  [(Or list1 list2)(reg-arith-op bit-or (evalFROL list1) (evalFROL list2))]                                                    ;; eval({or E1 E2}) = (<x1 bit-or y1> <x2 bit-or y2> ... <xk bit-or yk>,) where eval(E1) = (x1 x2 ... xk) and eval(E2) = (y1 y2 ... yk)

  [(FUN bound-id bound-body) (Fun->RES expr)]                                                                                  ;; eval(FUN) = FUN ; assuming FUN is a function expression     
  [(CALL fun-expr arg-expr)
   (let ([fval (RES->RegE (evalFROL fun-expr))])
     (cases fval
       [(FUN bound-id bound-body)                                                                                              ;; eval({call E1 E2}) = eval(Ef[eval(E2)/x])   if eval(E1) = {fun {x} Ef}
        (evalFROL (substFROL bound-body
                     bound-id
                     (RES->RegE (evalFROL arg-expr))))]
       [else (error 'evalFROL "`call' expects a function, got: ~s" fval)]))]                                                   ;; error!                 otherwise

  [(If l c r) (if (RegB->boolean (evalFROL l))                                                                                 ;; l - eval(true) = true / eval(false) = false
                  (evalFROL c)                                                                                                 ;; eval({if Econd Edo Eelse}) = eval(Edo) if eval(Econd) =/= false, = eval(Eelse), otherwise.
                  (evalFROL r))]
  [(ID name) (error 'evalFROL "free identifier: ~s" name)]                                                                     ;; symbol, error
  [(WITH bound-id named-expr bound-body) (cases (evalFROL named-expr)                                                          ;; eval({with {x E1} E2}) = eval(E2[eval(E1)/x])
                                           [(RegF id-name arg) (evalFROL (substFROL bound-body bound-id (FUN id-name arg)))]
                                           [(RegV BL) (evalFROL (substFROL bound-body bound-id (Reg BL)))]                     ;; for bit-list
                                           [(RegB BOOL) (evalFROL (substFROL bound-body bound-id (Bool BOOL)))])]))            ;; for boolean

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


(: RES->RegE : RES -> RegE)
;; extract a RegE from RES type
(define ( RES->RegE expr)
  (cases expr
    [(RegV list) (Reg list)]                                                            ;; Bit-List
    [(RegB BOOL) (Bool BOOL)]                                                           ;; boolean
    [(RegF id-name arg) (FUN id-name arg)]))                                            ;; Fun


(: Fun->RES : RegE -> RES)
;; extract a fun from RegE type
(define (Fun->RES expr)
  (cases expr
    [(FUN id-name arg) (RegF id-name arg)]                                               ;; Fun
    [else (error 'Fun->RES "any value instead of Fun in ~s" expr)]))                     ;; any value, error

;; new name because next questions
(: runFROL : String -> Bit-List)
;; evaluate a ROL program contained in a string
;; we will not allow to return a boolean type
(define (runFROL str)
  (let ([result (evalFROL (parseFROL str))])
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

;; test RES->RegE
(test (RES->RegE (RegB #t)) => (Bool #t))
(test (RES->RegE (RegV (list 1 1 1 0))) => (Reg (list 1 1 1 0)))
(test (RES->RegE (RegF 'x (Reg (list 1 1 1 0)))) => (FUN 'x (Reg (list 1 1 1 0))))

;; test Fun->RES
(test (Fun->RES (FUN 'x (Reg (list 1 1 1 0)))) => (RegF 'x (Reg (list 1 1 1 0))))
(test (Fun->RES (Bool #t)) =error> "any value instead of Fun in (Bool #t)")

;; tests
(test (runFROL "{ reg-len = 4 {1 0 0 0}}") => '(1 0 0 0))
(test (runFROL "{ reg-len = 4 {shl {1 0 0 0}}}") => '(0 0 0 1))
(test (runFROL "{ reg-len = 4 {and {shl {1 0 1 0}}{shl {1 0 1 0}}}}") => '(0 1 0 1))

;; tests
(test (runFROL "{ reg-len = 4 {1 0 0 0}}") => '(1 0 0 0))
(test (runFROL "{ reg-len = 4 {shl {1 0 0 0}}}") => '(0 0 0 1))
(test (runFROL "{ reg-len = 4 {and {shl {1 0 1 0}}{shl {1 0 1 0}}}}") => '(0 1 0 1))
(test (runFROL "{ reg-len = 4 {or {and {shl {1 0 1 0}} {shl {1 0 0 1}}} {1 0 1 0}}}") => '(1 0 1 1))
(test (runFROL "{ reg-len = 2 {or {and {shl {1 0}} {1 0}} {1 0}}}") => '(1 0))
(test (runFROL "{ reg-len = 4 {with {x {1 1 1 1}} {shl y}}}") =error> "free identifier: y")
(test (runFROL "{ reg-len = 2 {with {x { or {and {shl {1 0}} {1 0}} {1 0}}} {shl x}}}") => '(0 1))
(test (runFROL "{ reg-len = 4 {or {1 1 1 1} {0 1 1}}}") =error> "wrong number of bits in (0 1 1)")
(test (runFROL "{ reg-len = 0 {}}") =error> "Register length must be at least 1")
(test (runFROL "{ reg-len = 3 {if {geq? {1 0 1} {1 1 1}} {0 0 1} {1 1 0}}}") => '(1 1 0))
(test (runFROL "{ reg-len = 4 {if {maj? {0 0 1 1}} {shl {1 0 1 1}} {1 1 0 1}}}") => '(0 1 1 1))
(test (runFROL "{ reg-len = 4 {if false {shl {1 0 1 1}} {1 1 0 1}}}") => '(1 1 0 1))
(test (runFROL "{ reg-len = 2 {with {x false} {if x {1 1} {0 1}}}}") => '(0 1))

;; tests
(test (runFROL "{ reg-len = 3 {with {identity {fun {x} x}} {with {foo {fun {x} {or x {1 1 0}}}} {call {call identity foo} {0 1 0}}}}}") => '(1 1 0))
(test (runFROL "{ reg-len = 3 {with {x {0 0 1}} {with {f {fun {y} {and x y}}} {with {x {0 0 0}} {call f {1 1 1}}}}}}") => '(0 0 1))
(test (runFROL "{ reg-len = 4 {with {foo {fun {z} {if {maj? z} z {shl z}}}} {call foo {if {maj? {0 0 1 1}} {shl {1 0 1 1}} {1 1 0 1}}}}}") => '(0 1 1 1))


;;---------------------------------------------------------------------------------Question 2. Counting Free Instances of a Given Symbol--------------------------------------------------------------------------------------------
;;-----------------------------------------------------------------------------<<<FLANG>>>--------------------------------------------------------------------
;; The Flang interpreter – supporting both the substitution model and the substitution-cache model
#|
The grammar:
<FLANG> ::= <num>
| { + <FLANG> <FLANG> }
| { - <FLANG> <FLANG> }
| { * <FLANG> <FLANG> }
| { / <FLANG> <FLANG> }
| { with { <id> <FLANG> } <FLANG> }
| <id>
| { fun { <id> } <FLANG> }
| { call <FLANG> <FLANG> }
|#
(define-type FLANG
  [Num Number]
  [Add FLANG FLANG]
  [Sub FLANG FLANG]
  [Mul FLANG FLANG]
  [Div FLANG FLANG]
  [Id Symbol]
  [With Symbol FLANG FLANG]
  [Fun Symbol FLANG]
  [Call FLANG FLANG])

(: parse-sexpr : Sexpr -> FLANG)
;; to convert s-expressions into FLANGs
(define (parse-sexpr sexpr)
  (match sexpr
    [(number: n) (Num n)]
    [(symbol: name) (Id name)]
    [(cons 'with more)
     (match sexpr
       [(list 'with (list (symbol: name) named) body)
        (With name (parse-sexpr named) (parse-sexpr body))]
       [else (error 'parse-sexpr "bad `with' syntax in ~s" sexpr)])]
    [(cons 'fun more)
     (match sexpr
       [(list 'fun (list (symbol: name)) body)
        (Fun name (parse-sexpr body))]
       [else (error 'parse-sexpr "bad `fun' syntax in ~s" sexpr)])]
    [(list '+ lhs rhs) (Add (parse-sexpr lhs) (parse-sexpr rhs))]
    [(list '- lhs rhs) (Sub (parse-sexpr lhs) (parse-sexpr rhs))]
    [(list '* lhs rhs) (Mul (parse-sexpr lhs) (parse-sexpr rhs))]
    [(list '/ lhs rhs) (Div (parse-sexpr lhs) (parse-sexpr rhs))]
    [(list 'call fun arg) (Call (parse-sexpr fun) (parse-sexpr arg))]
    [else (error 'parse-sexpr "bad syntax in ~s" sexpr)]))

(: parse : String -> FLANG)
;; parses a string containing a FLANG expression to a FLANG AST
(define (parse str)
  (parse-sexpr (string->sexpr str)))

;;;;;; the evaluation part for the substitution model
(: subst : FLANG Symbol FLANG -> FLANG)
;; substitutes the second argument with the third argument in the
;; first argument, as per the rules of substitution; the resulting
;; expression contains no free instances of the second argument
(define (subst expr from to)
  (cases expr
    [(Num n) expr]
    [(Add l r) (Add (subst l from to) (subst r from to))]
    [(Sub l r) (Sub (subst l from to) (subst r from to))]
    [(Mul l r) (Mul (subst l from to) (subst r from to))]
    [(Div l r) (Div (subst l from to) (subst r from to))]
    [(Id name) (if (eq? name from) to expr)]
    [(With bound-id named-expr bound-body)
     (With bound-id
           (subst named-expr from to)
           (if (eq? bound-id from)
               bound-body
               (subst bound-body from to)))]
    [(Call l r) (Call (subst l from to) (subst r from to))]
    [(Fun bound-id bound-body)
     (if (eq? bound-id from)
         expr
         (Fun bound-id (subst bound-body from to)))]))

(: arith-op : (Number Number -> Number) FLANG FLANG -> FLANG)
;; gets a Racket numeric binary operator, and uses it within a FLANG
;; `Num' wrapper
(define (arith-op op expr1 expr2)
  (: Num->number : FLANG -> Number)
  (define (Num->number e)
    (cases e
      [(Num n) n]
      [else (error 'arith-op "expects a number, got: ~s" e)]))
  (Num (op (Num->number expr1) (Num->number expr2))))

(: eval : FLANG -> FLANG)
;; evaluates FLANG expressions by reducing them to *expressions*
(define (eval expr)
  (cases expr
    [(Num n) expr]
    [(Add l r) (arith-op + (eval l) (eval r))]
    [(Sub l r) (arith-op - (eval l) (eval r))]
    [(Mul l r) (arith-op * (eval l) (eval r))]
    [(Div l r) (arith-op / (eval l) (eval r))]
    [(With bound-id named-expr bound-body)
     (eval (subst bound-body
                  bound-id
                  (eval named-expr)))]
    [(Id name) (error 'eval "free identifier: ~s" name)]
    [(Fun bound-id bound-body) expr]
    [(Call fun-expr arg-expr)
     (let ([fval (eval fun-expr)])
       (cases fval
         [(Fun bound-id bound-body)
          (eval (subst bound-body
                       bound-id
                       (eval arg-expr)))]
         [else (error 'eval "`call' expects a function, got: ~s"
                      fval)]))]))

(: run : String -> Number)
;; evaluate a FLANG program contained in a string
(define (run str)
  (let ([result (eval (parse str))])
    (cases result
      [(Num n) n]
      [else (error 'run
                   "evaluation returned a non-number: ~s" result)])))
;; tests
(test (run "{call {fun {x} {+ x 1}} 4}") => 5)
(test (run "{with {add3 {fun {x} {+ x 3}}} {call add3 1}}") => 4)
(test (run "{with {add3 {fun {x} {+ x 3}}} {with {add1 {fun {x} {+ x 1}}} {with {x 3} {call add1 {call add3 x}}}}}") => 7)
(test (run "{with {identity {fun {x} x}} {with {foo {fun {x} {+ x 1}}} {call {call identity foo} 123}}}") => 124)
(test (run "{call {call {fun {x} {call x 1}} {fun {x} {fun {y} {+ x y}}}} 123}") => 124)



;; a function countFreeSingle that takes an abstract syntax tree for the FLANG language and a symbol,
;; and returns the number of free instances of the given symbol appear in the expression (tree).
(: countFreeSingle : FLANG Symbol -> Natural)
(define (countFreeSingle bound-body name)
  (cases  bound-body
    [(Num n) 0]                                                                                                ;; Num, return 0
    [(Add l r) (+ (countFreeSingle l name) (countFreeSingle r name))]                                          ;; Add, return countFreeSingle(E1 symbol) + countFreeSingle(E2 symbol)
    [(Sub l r) (+ (countFreeSingle l name) (countFreeSingle r name))]                                          ;; Sub, return countFreeSingle(E1 symbol) + countFreeSingle(E2 symbol)
    [(Mul l r) (+ (countFreeSingle l name) (countFreeSingle r name))]                                          ;; Mul, return countFreeSingle(E1 symbol) + countFreeSingle(E2 symbol)
    [(Div l r) (+ (countFreeSingle l name) (countFreeSingle r name))]                                          ;; Div, return countFreeSingle(E1 symbol) + countFreeSingle(E2 symbol)
    [(Id id-name) (cond
                    [(eq? name id-name) 1]                                                                     ;; id-name == symbol, return 1
                    [else 0])]                                                                                 ;; id-name != symbol, return 0
    [(With bound-id named-expr bound-body) (cond
                                             [(eq? name bound-id) (countFreeSingle bound-body name)]           ;; bound-id == symbol, return countFreeSingle(E2 symbol)
                                             [else  (+ (countFreeSingle bound-body name)                       ;; bound-id != symbol, return countFreeSingle(E1 symbol) + countFreeSingle(E2 symbol)
                                                       (countFreeSingle named-expr name))])]
    [(Fun bound-id bound-body) (cond
                                 [(eq? name bound-id) 0]                                                       ;; bound-id == symbol, return 0
                                 [else (countFreeSingle bound-body name)])]                                    ;; bound-id != symbol, return countFreeSingle(E1 symbol)
    [(Call fun-expr arg-expr) (+ (countFreeSingle fun-expr name)                                               ;; Call, return countFreeSingle(E1 symbol) + countFreeSingle(E2 symbol)
                                 (countFreeSingle arg-expr name))]))

(: CFSingle : String Symbol -> Natural)
;; the function call to the function countFreeSingle
(define (CFSingle expr name)
  (countFreeSingle (parse expr) name))

;; tests
(test (CFSingle "{+ r r}" 'r) => 2)
(test (CFSingle "{fun {r} {+ r e}}" 'e) => 1)
(test (CFSingle "{fun {r} {+ r e}}" 'r) => 0)
(test (CFSingle "{call {fun {r} {+ r e}} {with {e {+ e r}} {fun {x} {+ e r}}}}" 'r) => 2)
(test (CFSingle "{call {fun {r} {+ r e}} {with {e {+ e r}} {fun {x} {+ e r}}}}" 'e) => 2)
;;------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
;;a. An interesting test case
#|
Consider the following FLANG code:
   "{with {foo {fun {y} {+ x y}}}
            {with {x 4}
             {call foo 3}}}"
|#
;; CFSingle
;; What happens when you run the following code?
;; Answer: print 1
(CFSingle "{with {foo {fun {y} {+ x y}}} {with {x 4} {call foo 3}}}" 'x)

;; the FLANG interpreter (substitution model)
;; What happens when you run the FLANG interpreter (substitution model) on this code?
;; Answer: print 7
(run "{with {foo {fun {y} {+ x y}}} {with {x 4} {call foo 3}}}")

;; Explain what goes on and why it happens.
;; In CFSingle: In part one({foo {fun {y} {+ x y}}}) 'x is free because we define it as a foo.
;;              In the second part we define that X is equal to 4, so it is not free,
;;              and once a call occurs in a foo, X is changed to 4.
;;              parser also returns to us: (With foo (Fun y (Add (Id x) (Id y))) (With x (Num 4) (Call (Id foo) (Num 3))))
;;              As we can see 'x appears only one time free.
;;
;; In the FLANG interpreter (substitution model): In first with we define the FOO function, but do not know why Y and X are equal.
;;                                                In second with we define that x equal to 4 and call function foo with parameter 3 (y) thus,
;;                                                {+ 4 3} => 7

;;---------------------------------------------------------------------------------Question 3. Static versus Dynamic Scoping (recursion)--------------------------------------------------------------------------------------------
;;;;;; The evaluation part for the substitution cache model
;; a type for substitution caches:
(define-type SubstCache = (Listof (List Symbol FLANG)))
(: empty-subst : SubstCache)
(define empty-subst null)

(: extend : Symbol FLANG SubstCache -> SubstCache)
(define (extend name val sc)
  (cons (list name val) sc))

(: lookup : Symbol SubstCache -> FLANG)
(define (lookup name sc)
  (let ([cell (assq name sc)])
    (if cell
        (second cell)
        (error 'lookup "no binding for ~s" name))))

(: counterx : Natural)
(define counterx 0)

;;;above eval
(: evalSC : FLANG SubstCache -> FLANG)
;; evaluates FLANG expressions by reducing them to expressions
(define (evalSC expr sc)
  (set! counterx (add1 counterx))
  (if (> counterx 500)
      (error 'eval "exceeded 500 times")
      (cases expr
        [(Num n) expr]
        [(Add l r) (arith-op + (evalSC l sc) (evalSC r sc))]
        [(Sub l r) (arith-op - (evalSC l sc) (evalSC r sc))]
        [(Mul l r) (arith-op * (evalSC l sc) (evalSC r sc))]
        [(Div l r) (arith-op / (evalSC l sc) (evalSC r sc))]
        [(With bound-id named-expr bound-body)
         (evalSC bound-body
                 (extend bound-id (evalSC named-expr sc) sc))]
        [(Id name) (lookup name sc)]
        [(Fun bound-id bound-body) expr]
        [(Call fun-expr arg-expr)
         (let ([fval (evalSC fun-expr sc)])
           (cases fval
             [(Fun bound-id bound-body)
              (evalSC bound-body
                      (extend bound-id (evalSC arg-expr sc) sc))]
             [else (error 'evalSC "`call' expects a function, got: ~s"
                          fval)]))])))

(: runSC : String -> Number)
;; evaluate a FLANG program contained in a string
(define (runSC str)
  (let ([result (evalSC (parse str) empty-subst)])
    (cases result
      [(Num n) n]
      [else (error 'runSC
                   "evaluation returned a non-number: ~s" result)])))

(define loop "{with {Alexey {with {f {fun {x} {call f 1}}} {call f 0}}} 0}")
;; tests
(test (runSC loop) =error> "exceeded 500 times") ;; subst-cache model
(test (run loop) =error> "free identifier: f")   ;; substitution model