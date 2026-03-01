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

(add-variable "a" 40)
(add-variable "b" 41)
(add-variable "c" 42)
(add-variable "d" 43)
(println "Added variables:" variables)
(println "re-Tokenised input:" (tokenise inp))

(println "Parsed input:" (parse-infix (tokenise inp)))
