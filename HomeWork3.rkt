#lang pl 03
;;*******************************************************************************************
;;*                                 Assignmet 3                                             *
;;* Author: Alexey Titov                                                                    *
;;* Date: 05/2019                                                                           *
;;* Version: 1.0                                                                            *
;;*******************************************************************************************

;;---------------------------------------------------------------------------------Question 1. Adding with expressions to the ROL language--------------------------------------------------------------------------------------------
#| BNF for the ROL language:

   <ROL> ::= {reg-len= <num> <RegE>}

   <RegE> ::= <Bits>
             |{and <RegE> <RegE>} 
             |{or <RegE> <RegE>} 
             |{shl <RegE>} 
             |{with {<ID> <RegE> }<RegE>}
             |{<ID>}
             |{geq? <RegE> <RegE>}
             |{maj? <RegE>}
             |{if <RegE> <RegE> <RegE>}
             |<Bool>


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
  [Id Symbol]
  [With Symbol RegE RegE]
  [Bool Boolean]
  [Geq RegE RegE]
  [Maj RegE]
  [If RegE RegE RegE])

;; Next is a technical function that converts (casts)
;; (any) list into a bit-list. We use it in parse-sexpr.
(: list->bit-list : (Listof Any) -> Bit-List)
;; to cast a list of bits as a bit-list
(define (list->bit-list lst)
  (cond [(null? lst) null]
        [(eq? (first lst) 1)(cons 1 (list->bit-list (rest lst)))]
        [else (cons 0 (list->bit-list (rest lst)))]))

(: parse-sexpr : Sexpr -> RegE)
;; to convert the main s-expression into ROL
(define (parse-sexpr sexpr)
  (match sexpr
    [(list 'reg-len '= (number: n) args)
     (if (> n 0)                                                                  ;; remember to make sure specified register length is at least 1
         (parse-sexpr-RegL args n)
         (error 'parse-sexpr "Register length must be at least 1 ~s" sexpr))]     ;; error, n < 1
    [else (error 'parse-sexpr "bad syntax in ~s" sexpr)]))                        ;; error, syntax

(: parse-sexpr-RegL : Sexpr Number -> RegE)
;; to convert s-expressions into RegEs
(define (parse-sexpr-RegL sexpr reg-len)
  (match sexpr
    [(list (and a (or 1 0)) ... ) (if (= reg-len (length a))
                                      (Reg (list->bit-list a))                                                                                    ;; Reg
                                      (error 'parse-sexpr "wrong number of bits in ~s" a))]                                                       ;; error of Reg, len(bit-list)!=n
    [(list 'and list1 list2) (And (parse-sexpr-RegL list1 reg-len) (parse-sexpr-RegL list2 reg-len))]                                             ;; And
    [(list 'or list1 list2) (Or (parse-sexpr-RegL list1 reg-len) (parse-sexpr-RegL list2 reg-len))]                                               ;; Or
    [(list 'shl list) (Shl (parse-sexpr-RegL list reg-len))]                                                                                      ;; Shl
    [(boolean: bool-value) (Bool bool-value)]                                                                                                     ;; Bool
    [(symbol: id-name) (cond [(eq? id-name 'false) (Bool false)]                                                                                  ;; Bool false
                             [(eq? id-name 'true) (Bool true)]                                                                                    ;; Bool true
                             [else (Id id-name)])]                                                                                                ;; Id
    [(cons 'with args)             
     (match sexpr
       [(list 'with (list(symbol: oldName) newName) body)                                                                                         ;; With 
        (With oldName (parse-sexpr-RegL newName reg-len) (parse-sexpr-RegL body reg-len))]                  
       [else (error 'parser-sexpr-RegL "bad `with' syntax in ~s" sexpr)])]                                                                        ;; error of With 
    [(list 'geq? list1 list2) (Geq(parse-sexpr-RegL list1 reg-len) (parse-sexpr-RegL list2 reg-len))]                                             ;; Geq
    [(list 'maj? list) (Maj (parse-sexpr-RegL list reg-len))]                                                                                     ;; Maj
    [(list 'if list1 list2 list3) (If (parse-sexpr-RegL list1 reg-len) (parse-sexpr-RegL list2 reg-len) (parse-sexpr-RegL list3 reg-len))]        ;; If
    [else (error 'parse-sexpr "bad syntax in ~s" sexpr)]))                                                                                        ;; error of parse-sexpr-RegL

(: parse : String -> RegE)
;; parses a string containing a RegE expression to a RegE AST
(define (parse str)
  (parse-sexpr (string->sexpr str)))

;;---------------------------------------------------------------------------------Question 2. Supported Types--------------------------------------------------------------------------------------------

(define-type RES
  [RegV Bit-List]                       ;; register output
  [RegB Boolean])                       ;; boolean output

;;---------------------------------------------------------------------------------Question 3. Substitutions--------------------------------------------------------------------------------------------
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
|#

(: subst : RegE Symbol RegE -> RegE)
;; substitutes the second argument with the third argument in the
;; first argument, as per the rules of substitution; the resulting
;; expression contains no free instances of the second argument
(define (subst expr from to)
  (cases expr
    [(Reg BL) expr]                                                             ;; BL[v/x] = BL
    [(Bool BOOL) expr]                                                          ;; BOOL[v/x] = BOOL
    [(And l r) (And (subst l from to) (subst r from to))]                       ;; {and E1 E2}[v/x] = {and E1[v/x] E2[v/x]}
    [(Or l r) (Or (subst l from to) (subst r from to))]                         ;; {or E1 E2}[v/x] = {or E1[v/x] E2[v/x]}
    [(Geq l r) (Geq (subst l from to) (subst r from to))]                       ;; {geq? E1 E2}[v/x] = {geq? E1[v/x] E2[v/x]}
    [(Shl l) (Shl (subst l from to))]                                           ;; {shl E}[v/x] = {shl E[v/x]}
    [(Maj l) (Maj (subst l from to))]                                           ;; {maj? E1}[v/x] = {maj? E1[v/x] }
    [(If l c r) (If (subst l from to) (subst c from to) (subst r from to))]     ;; {if {E1} E2 E3}[v/x] = {if {E1[v/x]} E2[v/x] E3[v/x]}
    [(Id name) (if (eq? name from) to expr)]                                    ;; y[v/x] = y       ||  x[v/x] = v
    [(With bound-id named-expr bound-body)
     (With bound-id
           (subst named-expr from to)
           (if (eq? bound-id from)
               bound-body                                                       ;; {with {x E1} E2}[v/x] = {with {x E1[v/x]} E2}
               (subst bound-body from to)))]))                                  ;; {with {y E1} E2}[v/x] = {with {y E1[v/x]} E2[v/x]}


;;---------------------------------------------------------------------------------Question 4. Evaluation--------------------------------------------------------------------------------------------
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
|#


(: eval : RegE -> RES)
;; evaluates RegE expressions by reducing them to bit-lists
(define (eval expr)
(cases expr
  [(Reg BL) (RegV BL)]                                                                                                         ;; eval(Reg) = Reg
  [(Bool BOOL) (RegB BOOL)]                                                                                                    ;; eval(bl) = bl (eval(true) = true / eval(false) = false)
  [(Shl list) (RegV (shift-left (RegV->bit-list (eval list))))]                                                                ;; eval({shl E}) = (x2 ... xk x1), where eval(E) = (x1 x2 ... xk)
  [(Maj list) (RegB (majority? (RegV->bit-list (eval list))))]                                                                 ;; eval({maj? E}) = true if x1+x2+...+xk >= k/2, and false otherwise, where eval(E) = (x1 x2 ... xk)
  [(Geq list1 list2) (RegB (geq-bitlists? (RegV->bit-list (eval list1)) (RegV->bit-list (eval list2))))]                       ;; eval({geq? E1 E2}) = true if x_i >= y_i, where eval(E1) = (x1 x2 ... xk) and eval(E2) = (y1 y2 ... yk) and i is the first index s.t. x_i and y_i are not equal (or i =k if all are equal)
  [(And list1 list2)(reg-arith-op bit-and (eval list1) (eval list2))]          ;; eval({and E1 E2}) = (<x1 bit-and y1> <x2 bit-and y2> ... <xk bit-and yk>), where eval(E1) = (x1 x2 ... xk) and eval(E2) = (y1 y2 ... yk)
  [(Or list1 list2)(reg-arith-op bit-or (eval list1) (eval list2))]            ;; eval({or E1 E2}) = (<x1 bit-or y1> <x2 bit-or y2> ... <xk bit-or yk>,) where eval(E1) = (x1 x2 ... xk) and eval(E2) = (y1 y2 ... yk)
  [(If l c r) (if (RegB->boolean (eval l))                                                                                     ;; l - eval(true) = true / eval(false) = false
                  (eval c)                                                                                                     ;; eval({if Econd Edo Eelse}) = eval(Edo) if eval(Econd) =/= false, = eval(Eelse), otherwise.
                  (eval r))]
  [(Id name) (error 'eval "free identifier: ~s" name)]                                                                         ;; symbol, error
  [(With bound-id named-expr bound-body) (cases (eval named-expr)                                                              ;; eval({with {x E1} E2}) = eval(E2[eval(E1)/x])
                                           [(RegV BL) (eval (subst bound-body bound-id (Reg BL)))]                             ;; for bit-list
                                           [(RegB BOOL) (eval (subst bound-body bound-id (Bool BOOL)))])]))                    ;; for boolean

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
      1                     ;; 1 and 1       /  1 and 0        /   0 and 1 
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
(define(shift-left bl)
  (append (cdr bl) (cons (car bl) '())))                                              ;; to replace the first cons with an append for adding the first element at the end of the list


(: RegV->bit-list : RES -> Bit-List)
;; extract a bit-list from RES type
(define (RegV->bit-list expr)
  (cases expr
    [(RegV list) list]                                                                               ;; Bit-List
    [(RegB BOOL) (error 'RegV->bit-lis "boolean value instead of Bit-List in ~s" expr)]))            ;; boolean, error

(: RegB->boolean : RES -> Boolean)
;; extract a boolean from RES type
(define (RegB->boolean expr)
  (cases expr
    [(RegB BOOL) BOOL]                                                                               ;; boolean
    [(RegV list) (error 'RegB->boolean "Bit-List instead of boolean in ~s" expr)]))                  ;; Bit-List, error

;;---------------------------------------------------------------------------------Question 5. Interface--------------------------------------------------------------------------------------------
(: run : String -> Bit-List)
;; evaluate a ROL program contained in a string
;; we will not allow to return a boolean type
(define (run str)
  (RegV->bit-list (eval (parse str))))



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

;; tests
(test (run "{ reg-len = 4 {1 0 0 0}}") => '(1 0 0 0))
(test (run "{ reg-len = 4 {shl {1 0 0 0}}}") => '(0 0 0 1))
(test (run "{ reg-len = 4 {and {shl {1 0 1 0}}{shl {1 0 1 0}}}}") => '(0 1 0 1))

;; tests
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
