#lang pl
;;*******************************************************************************************
;;*                                 Assignmet 1                                             *
;;* Author: Alexey Titov                                                                    *
;;* Date: 03/2019                                                                           *
;;* Version: 1.0                                                                            *
;;*******************************************************************************************

;;---------------------------------------------------------------------------------Question 1--------------------------------------------------------------------------------------------
#|
  the function search word with prefix "pl" in list of strings
  and returns the first one that contains the string "pl" as a prefix – if one such
  exists, and returns #f otherwise
  listStr - a list of string
|#
(: helperPlPrefixContained : (Listof String)-> (U String Boolean) )
(define (helperPlPrefixContained listStr)
  (if (null? listStr)                                                                                                                       ;; call function null? to check for empty list 
      #f
      (cond                                       
        [(> (string-length (list-ref listStr 0)) 2) (cond                                                                                   ;; checks if a word length is greater than two
                                                      [(string=? (substring (list-ref listStr 0) 0 2) "pl") (list-ref listStr 0)]           ;; checks if a prefix of word is "pl"
                                                      [else (helperPlPrefixContained (rest listStr))])]                                     ;; move to the next word
        [else (helperPlPrefixContained (rest listStr))])))                                                                                  ;; move to the next word
  
;; the function checks whether a list meets the requirements, if so call functon helperPlPrefixContained, and return error otherwise
;; listStr - a list of string
(: plPrefixContained : (Listof String)-> (U String Boolean) )
(define (plPrefixContained listStr)
  (cond
    [(= (length listStr) 5) (helperPlPrefixContained listStr)]                                                                 ;; checks if length list is 5
    [else (error 'plPrefixContained "you need entry list in length 5")]))                                                      ;; throw error

;; tests --- plPrefixContained/helperPlPrefixContained
(test (plPrefixContained (list "yyyt" "TplT" "plTT" "PlPl" "plplpl")) => "plTT")
(test (plPrefixContained (list "yypl" "opl" "lpTT" "lpl" "lol")) => false)
(test (plPrefixContained (list "yypl" "opl" "lpTT" "lpl")) =error> "plPrefixContained: you need entry list in length 5")
(test (helperPlPrefixContained '()) => false)
(test (helperPlPrefixContained (list "yyyt" "TplT" "plTT" "PlPl" "plplpl")) => "plTT")
;;---------------------------------------------------------------------------------Question 2--------------------------------------------------------------------------------------------
;; 2.a)
;; the function returns the longest string between 'one' and 'list-ref ls 0'
;; one - can be a string or #f
;; ls - a list of string
(: longest : (U Boolean String) (Listof String)-> String)
(define (longest one ls)
  (define two (list-ref ls 0))
  (cond
    [(not (string? one)) two]                                              ;; check if 'one' is string, if not return 'list-ref ls 0' because 'list-ref ls 0' is first string
    [else (if (< (string-length one) (string-length two)) two one)]))      ;; return the longest string

;; this function helps to longestString be tail-recursion
;; ans - can be a string or #f
;; ls - a list of string
(: helper-longestString : (U String Boolean) (Listof String)-> (U String Boolean) )
(define (helper-longestString ans ls)
  (if(null? ls)                                                            ;; call function null? to check for empty list 
     ans
     (helper-longestString  (longest ans ls) (rest ls))))                  ;; move to the next element
#|
  the function longestString that consumes a list (may contain
  elements of any type) and returns the longest string that is an
  element of this list, and #f if no such string exists
  lst - a list of any variables
|#
(: longestString : (Listof Any)-> (U String Boolean))
(define (longestString lst)
  (helper-longestString #f (filter string? lst)))                          ;; #f - there was not a string yet,
                                                                           ;; 'filter string? lst' returns a list with the elements of lst for which  string? produces a true value.
                                                                           ;; The string? procedure is applied to each element from first to last.

;; tests --- longestString/longest/helper-longestString
(test (longestString '(34 uuu 90)) => false)            
(test (longestString '(uu 56 oooo "r" "rRR" "TTT")) => "rRR")
(test (longestString '()) => false)
(test (longest #f '("r" "rRR" "TTT")) => "r")
(test (longest "one" '("r" "rRR" "TTT")) => "one")
(test (helper-longestString #f '()) => false)
(test (helper-longestString #f '("r" "rRR" "TTT")) => "rRR")

;; 2.b)
;; the function returns the shortest string between 'one' and 'list-ref ls 0'
;; one - can be a string or #f
;; ls - a list of string
(: shortest : (U String Boolean) (Listof String)-> String)
(define (shortest one ls)
  (define two (list-ref ls 0))
  (cond
    [(not (string? one)) two]                                              ;; check if 'one' is string, if not return 'list-ref ls 0' because 'list-ref ls 0' is first string
    [else (if (> (string-length one) (string-length two)) two one)]))      ;; return the shortest string
#|
  the function longestString that consumes a list
  and returns the shortest string that is an
  element of this list
  ans - can be a string or #f
  ls - a list of string
|#
(: helper-shortestString : (U String Boolean) (Listof String)-> (U String Boolean))
(define (helper-shortestString ans ls)
  (if(null? ls)                                                            ;; call function null? to check for empty list 
     ans
     (helper-shortestString  (shortest ans ls) (rest ls))))                ;; move to the next element

#|
  this function calls helper-shortestString and helper-longestString functions and puts their answers into a list that it returns
  lst - a list of string
|#
(: short&long : (Listof String)-> (Listof Any))
(define (short&long lst)
  (list (helper-shortestString #f lst)  (helper-longestString #f lst)))

;; this function helps to short&long-lists be tail-recursion
;; newls - new list of lists after filtering
;; oldls - the list before filtering
(: helper-short&long-lists : (Listof Any)(Listof (Listof Any))-> (Listof Any))
(define (helper-short&long-lists newls oldls)
  (if (null? oldls)                                                                                                     ;; call function null? to check for empty list 
      (reverse newls)                                                                                                   ;; return answer, we have added lists to the beginning of the list of lists and therefore need to be reversed.
      (if (ormap string? (list-ref oldls 0))                                                                            ;; ormap string? returns true if there is even one string in the list
          (helper-short&long-lists (cons (short&long (filter string? (list-ref oldls 0))) newls) (rest oldls))          ;; call to short&long function with a list after 'filter string?'
          (helper-short&long-lists (cons '() newls) (rest oldls)))))                                                    ;; there is no string in the list, so an empty list is added

#|
  this function that consumes a list of lists (where the type of the elements in the inner lists may be any type).
  The function returns a list of lists – such that for each inner list lst (in the original list) the following is done –
    1. If lst contains at least one string, then lst is replaced with a list of size two, containing the shortest string and the longest string in lst, and
    2. Otherwise, lst is replaced with a null.
  listOfls - a list of lists
|#
(: short&long-lists : (Listof (Listof Any))->(Listof Any))
(define (short&long-lists listOfls)
  (helper-short&long-lists '() listOfls))

;; tests --- short&long-lists/helper-short&long-lists/short&long/shortest/helper-shortestString
(test (short&long-lists '((any "Benny" 10 "OP" 8) (any Benny OP (2 3)))) => '(("OP" "Benny") ()))
(test (short&long-lists '(("2 5 5" 1 "5gg" L) (v gggg "f") ())) => '(("5gg" "2 5 5") ("f" "f") ()))
(test (short&long-lists '((any Benny OP (2 3)) (any "Benny" 10 "OP" 8))) => '(() ("OP" "Benny")))
(test (short&long-lists '(("2 5 5" 1 "5gg" L) (v gggg "f") () (any "Benny" 10 "OP" 8))) => '(("5gg" "2 5 5") ("f" "f") () ("OP" "Benny")))
(test (helper-short&long-lists '() '((any "Benny" 10 "OP" 8) (any Benny OP (2 3)))) => '(("OP" "Benny") ()))
(test (helper-short&long-lists '() '(("2 5 5" 1 "5gg" L) (v gggg "f") ())) => '(("5gg" "2 5 5") ("f" "f") ()))
(test (short&long '()) => '(#f #f))
(test (short&long '("one")) => '("one" "one"))
(test (short&long '("0" "one" "1234")) => '("0" "1234"))
(test (shortest #f '("r" "rRR" "TTT")) => "r")
(test (shortest "one" '("r" "rRR" "TTT")) => "r")
(test (helper-shortestString #f '()) => false)
(test (helper-shortestString #f '("r" "rRR" "TTT")) => "r")


;;---------------------------------------------------------------------------------Question 3--------------------------------------------------------------------------------------------
;; 3.1-2)
#|
  KeyStack - a new type. Each element in the stack will be keyed (indexed) with a symbol.
  operation Push – this a variant of the data type. The push operation should take as input a symbol (key),
  a string (value), and an existing keyed-stack and return an extended keystack in the natural way.
|#
(define-type KeyStack
  [EmptyKS]                            ;; the empty stack
  [Push Symbol String KeyStack])       ;; operation

;; 3.3)
#|
  search-stack – the search operation should take as input a symbol (key) and a keyed-stack and return the first (LIFO, last in first out) value that is keyed accordingly.
  If the key does not appear in the original stack, it should return a #f value.
  symb - the symbol to be found
  stack - the stack where function need to find
|#
(: search-stack : Symbol KeyStack -> (U Boolean String))
(define (search-stack symb stack)
  (cases stack
    [(EmptyKS) #f]                                     ;; the stack is empty
    [(Push sy st ks)(if (eq? symb sy)                  ;; sy - a symbol from stack, if sy == symb 
                        st                             ;; yes, return value (st)
                        (search-stack symb ks))]))     ;; no,  move to the next element 

;; 3.4)
#|
  pop-stack – the pop operation should take as input a keyed-stack and return the keyed-stack without its first (keyed) value.
  If the original stack was empty, it should return a #f value.
  stack - a stack of KeyStack
|#
(: pop-stack : KeyStack -> (U Boolean KeyStack))
(define (pop-stack stack)
  (cases stack
    [(EmptyKS) #f]                             ;; the stack is empty
    [(Push sy st ks) ks]))                     ;; return the keyed-stack without its first value

;; tests --- EmptyKS/Push/search-stack/pop-stack
(test (EmptyKS) => (EmptyKS))
(test (Push 'b "B" (Push 'a "A" (EmptyKS))) => (Push 'b "B" (Push 'a "A" (EmptyKS))))
(test (Push 'a "AAA" (Push 'b "B" (Push 'a "A" (EmptyKS)))) => (Push 'a "AAA" (Push 'b "B" (Push 'a "A" (EmptyKS)))))
(test (search-stack 'a (Push 'a "AAA" (Push 'b "B" (Push 'a "A" (EmptyKS))))) => "AAA")
(test (search-stack 'c (Push 'a "AAA" (Push 'b "B" (Push 'a "A" (EmptyKS))))) => #f)
(test (pop-stack (Push 'a "AAA" (Push 'b "B" (Push 'a "A" (EmptyKS))))) => (Push 'b "B" (Push 'a "A" (EmptyKS))))
(test (pop-stack (EmptyKS)) => #f)

;;---------------------------------------------------------------------------------Question 4--------------------------------------------------------------------------------------------
(: is-odd? : Natural -> Boolean)
#|
  this function and is-even? working in recursive form and
  returns true if the number x is odd, otherwise false
  if x is odd, we need do subtraction odd times, so we stop on 'if (zero? x)' in is-even? - return true
  otherwise we do subtraction even times and stop 'if (zero? x)' in is-odd? - return false
  x - natural number
|#
(define (is-odd? x)
  (if (zero? x)                      ;; determines if some number is zero or not
      false                          ;; 0 is odd
      (is-even? (- x 1))))           ;; is-even?(x - 1)

(: is-even? : Natural -> Boolean)
#|
 this function and is-odd? working in recursive form and
 returns true if the number x is even, otherwise false
 if x is even, we need do subtraction even times, so we stop on 'if (zero? x)' in is-even? - return true
 otherwise we do subtraction odd times and stop 'if (zero? x)' in is-odd? - return false
 x - natural number
|#
(define (is-even? x)
  (if (zero? x)                      ;; determines if some number is zero or not
      true                           ;; 0 is odd
      (is-odd? (- x 1))))            ;; is-odd?(x - 1)

;; tests --- is-odd?/is-even?
(test (not (is-odd? 12)))         ;; return false, because 12 is even
(test (is-even? 12))              ;; return true, because 12 is even
(test (not (is-odd? 0)))          ;; return false, because 0 is even
(test (is-even? 0))               ;; return true, because 0 is even
(test (is-odd? 1))                ;; return true, because 1 is odd
(test (not (is-even? 1)))         ;; return false, because 1 is odd

(: every? : (All (A) (A -> Boolean) (Listof A) -> Boolean))
#|
  see explanation about the All syntax at the end of the file...
  The recursive function that checks each member in list is even
  return true if list is empty or each member in list is even, otherwise false
  pred - a function, that return boolean
  lst - a list of A variables
|#
(define (every? pred lst)
  (or (null? lst)                           ;; list is empty
      (and (pred (first lst))               ;; check whether first element from list is even 
           (every? pred (rest lst)))))      ;; a recursive call

;; An example for the usefulness of this polymorphic function
(: all-even? : (Listof Natural) -> Boolean)
#|
  this function checks whether in the list all the natural numbers are even
  she call function every? and post as parameters  'is-even?' function and list
  lst - a list of A variables
|#
(define (all-even? lst)
  (every? is-even? lst))

;; tests --- all-even?
(test (all-even? null))                      ;; return true, because 'or (null? lst)' is true
(test (all-even? (list 0)))                  ;; return true, because '0' is even and it is single in list
(test (all-even? (list 2 4 6 8)))            ;; return true, because '2 4 6 8' are even
(test (not (all-even? (list 1 3 5 7))))      ;; return false, because '1 3 5 7' are odd
(test (not (all-even? (list 1))))            ;; return false, because '1' is odd and it is single in list
(test (not (all-even? (list 2 4 1 6))))      ;; return false, because '1' is odd

(: every2? : (All (A B) (A -> Boolean) (B -> Boolean) (Listof A) (Listof B) -> Boolean))
#|
  this recursive function checks whether in the first list each number is pred1
  and in the second list each number is pred2. Two lists should be the same lenght
  pred1 - a function, that return boolean
  pred2 - a function, that return boolean
  lst1 - a list of A variables
  lst2 - a list of B variables
|#
(define (every2? pred1 pred2 lst1 lst2)
  (or (null? lst1)                                             ;; both lists assumed to be of same length
      (and (pred1 (first lst1))                                ;; check whether first element from lst1 is pred1
           (pred2 (first lst2))                                ;; check whether first element from lst2 is pred2
           (every2? pred1 pred2 (rest lst1) (rest lst2)))))    ;; a recursive call

;; tests --- every2?
(test (every2? is-even? is-odd? null (list 1 3 4 5)))                  ;; return true, because 'or (null? lst1)' is true
(test (every2? is-even? is-odd? (list 2 4 6) (list 1 3 5)))            ;; return true, because '2 4 6' is even (pred1 'is-even?')  and '1 3 5' is odd (pred2 'is-odd?') 
(test (every2? is-even? is-even? (list 2 4 6) (list 8 10 12)))         ;; return true, because '2 4 6' is even (pred1 'is-even?')  and '8 10 12' is even (pred2 'is-even?') 
(test (not (every2? is-even? is-odd? (list 1 3 5) (list 2 4 6))))      ;; return false, because '1 3 5' is odd (pred1 'is-even?')  and '2 4 6' is even (pred2 'is-odd?') 
(test (not (every2? is-even? is-odd? (list 2) (list 2))))              ;; return false, because '2' is even (pred1 'is-even?')  and '2' is even (pred2 'is-odd?') 
(test (not (every2? is-even? is-odd? (list 2 4 6 8) (list 1 3 4 5))))  ;; return false, because '2 4 6 8' is even (pred1 'is-even?')  and '4' from '1 3 4 5' is odd (pred2 'is-odd?') 