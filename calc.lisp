; run this file with ./rho calc.lisp
; a calculator program for the first "Monthly" Computer Science Project

(include "utils.lisp")
(include "tokenise.lisp")
(include "parse.lisp")

(println "Currently got the follwing symbols defined:" syms)

(write stdout "> ")
(def inp (readline stdin))

(println "Got input:" inp)
(println "Tokenised input:" (tokenise inp))

(add-variable "a" 3)
(println "Added variable `a`:" variables)
(println "re-Tokenised input:" (tokenise inp))

(println "Parsed input:" (parse-infix (tokenise inp)))
