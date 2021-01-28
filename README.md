NUMEX
==
A Functional Intrepreter written in Racket for Programming Languages Final Project.

> Warning: This project is far from perfect (especially the challenge part), but it somehow passed all the test cases.

Overview
=================
This project has to do with NUMEX (**Num**ber-**Ex**pression Programming Language). NUMEX programs are written directly in Racket by using the constructors defined by the structs defined at the beginning of project.rkt (Note: you must define missing ones). Here is the definition of NUMEX's syntax:
- If 𝑠 is a Racket string, then (var 𝑠) is a NUMEX expression (variables).
- If 𝑛 is a Racket integer, then (num 𝑛) is a NUMEX expression (number constants).
- If 𝑏 is a Racket boolean, then (bool 𝑏) is a NUMEX expression (boolean constants).
- If 𝑒1 and 𝑒2 are NUMEX expressions, then (plus 𝑒1 𝑒2) is a NUMEX expression (addition).
- If 𝑒1 and 𝑒2 are NUMEX expressions, then (minus 𝑒1 𝑒2) is a NUMEX expression (subtraction).
- If 𝑒1 and 𝑒2 are NUMEX expressions, then (mult 𝑒1 𝑒2) is a NUMEX expression (multiplication).
- If 𝑒1 and 𝑒2 are NUMEX expressions, then (div 𝑒1 𝑒2) is a NUMEX expression (division).
- If 𝑒1 is a NUMEX expression, then (neg 𝑒1) is a NUMEX expression (negation).
- If 𝑒1 and 𝑒2 are NUMEX expressions, then (andalso 𝑒1 𝑒2) is a NUMEX expression (logical conjunction).
- If 𝑒1 and 𝑒2 are NUMEX expressions, then (orelse 𝑒1 𝑒2) is a NUMEX expression (logical disjunction).
- If 𝑒1, 𝑒2, and 𝑒3 are NUMEX expressions, then (cnd 𝑒1 𝑒2 𝑒3) is a NUMEX expression. It is a condition where the result is 𝑒2 if 𝑒1 is true, else the result is 𝑒3. Only one of 𝑒2 and 𝑒3 is evaluated.
- If 𝑒1 and 𝑒2 are NUMEX expressions, then (iseq 𝑒1 𝑒2) is a NUMEX expression. (comparison).
- If 𝑒1, 𝑒2, and 𝑒3 are NUMEX expressions, then (ifnzero 𝑒1 𝑒2 𝑒3) is a NUMEX expression. It is a condition where the result is 𝑒2 if 𝑒1 is not zero, else the result is 𝑒3. Only one of 𝑒2 and 𝑒3 is evaluated.
If 𝑒1, 𝑒2, 𝑒3, and 𝑒4 are NUMEX expressions, then (ifleq 𝑒1 𝑒2 𝑒3 𝑒4) is a NUMEX expression. It is a conditional where the result is 𝑒4 if 𝑒1 is strictly greater than 𝑒2, else the result is 𝑒3. Only one of 𝑒3 and 𝑒4 is evaluated.
- If 𝑠1 and 𝑠2 are Racket strings and 𝑒 is a NUMEX expression, then (lam 𝑠1 𝑠2 𝑒) is a NUMEX expression (a function). In 𝑒, 𝑠1 is bound to the function itself (for recursion) and 𝑠2 is bound to the only argument. Also, (lam null 𝑠2 𝑒) is allowed for anonymous nonrecursive functions.
- If 𝑒1 and 𝑒2 are NUMEX expressions, then (apply 𝑒1 𝑒2) is a NUMEX expression (function application).
- If 𝑠 is a Racket string, and 𝑒1 and 𝑒2 are NUMEX expressions, then (with 𝑠 𝑒1 𝑒2) is a NUMEX expression (a let expression where the value of 𝑒1 is bound to 𝑠 in 𝑒2).
- If 𝑒1 and 𝑒2 are NUMEX expressions, then (apair 𝑒1 𝑒2) is a NUMEX expression (pair constructor).
- If 𝑒1 is a NUMEX expression, then (1st 𝑒1) is a NUMEX expression (the first part of a pair).
- If 𝑒1 is a NUMEX expression, then (2nd 𝑒1) is a NUMEX expression (the second part of a pair).
- (munit) is a NUMEX expression (holding no data, much like () in ML or null in Racket). Notice (munit) is a NUMEX expression, but munit is not.
- If 𝑒1 is a NUMEX expression, then (ismunit 𝑒1) is a NUMEX expression (testing for (munit)).
- (closure 𝑒𝑛𝑣 𝑓) is a NUMEX value where 𝑓 is a NUMEX function and 𝑒𝑛𝑣 is an environment that maps variables to values. Closures do not appear in programs; they result from evaluating functions.
- If 𝑠1 is a Racket string and 𝑠2 is a Racket string and 𝑠3 is a Racket string and 𝑠4 is a Racket string and 𝑒1 is a NUMEX expression and 𝑒2 is NUMEX expression and 𝑒3 is a NUMEX expression and 𝑒4 is NUMEX expression and 𝑒5 is NUMEX expression, then (letrec 𝑠1 𝑒1 𝑠2 𝑒2 𝑠3 𝑒3 𝑠4 𝑒4 𝑒5) is a NUMEX expression (a letrec expression for recursive definitions where the the value of 𝑒1 is bound to 𝑠1 and the value of 𝑒2 is bound to 𝑠2 and the value of 𝑒3 is bound to 𝑠3 and the value of 𝑒4 is bound to 𝑠4 in the 𝑒5).
- If s is a Racket string and e is a NUMEX expression, then (key s e) is a NUMEX expression (key contructor).
- If k is a NUMEX key and m is a NUMEX munit, then (record k m) is a NUMEX expression (record contructor).
- If k is a NUMEX key and r is a NUMEX record, then (record k r) is a NUMEX expression (record constructor).
- If s is a Racket string and r is a NUMEX record, then (value s r) is a NUMEX expression (value of string in record).

A NUMEX 𝑣𝑎𝑙𝑢𝑒 is a NUMEX number constant, a NUMEX boolean constant, a NUMEX closure, a NUMEX munit, or a NUMEX pair of NUMEX values. Similar to Racket, we can build list values out of nested pair values that end with a NUMEX munit. Such a NUMEX value is called a NUMEX list.

You should 𝑛𝑜𝑡 assume NUMEX programs are syntactically correct (e.g., things like (num "hi") or (num (num 37)) must be handled). And do 𝑛𝑜𝑡 assume NUMEX programs are free of type errors like (plus (munit) (num 7)), (1st (num 7)) or (div (bool #t) (num 2)).
