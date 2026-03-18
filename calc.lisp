; run this file with ./rho calc.lisp
; a calculator program for the first "Monthly" Computer Science Project

(include "utils.lisp")
(include "tokenise.lisp")
(include "parse.lisp")
(include "calculate.lisp")

(write stdout "> ")
(def inp (readline stdin))

(println "Got input:" inp)
(println "Tokenised input:" (tokenise inp))

(add-variable "a" '(over 40 1))
(add-variable "b" '(over 41 1))
(add-variable "c" '(over 42 1))
(add-variable "d" '(over 43 1))
(println "Added variables:" variables)
(println "re-Tokenised input:" (tokenise inp))

(println "Parsed input:" (parse-infix (tokenise inp)))

(defn rational.repr (rat)
  (if (= (nth rat 2) 1)
    (repr (scd rat))
    (&$ (repr (scd rat)) # / (repr (nth rat 2)))
  )
)
(defn rational.decimal (rat)
  (&$ (repr (/ (scd rat) (nth rat 2))) # . (tmpfn (acc digits num den)
    (if digits
      (rec
        (&$ acc (+ # 0 (/ (* num 10) den)))
        (- digits 1)
        (- (* num 10) (* (/ (* num 10) den) den))
        den
      )
      acc
    )
    ("" 6 (- (scd rat) (* (/ (scd rat) (nth rat 2)) (nth rat 2))) (nth rat 2))
  ))
)

(defn show-rational (rat)
  (&$ (rational.repr rat) " (" (rational.decimal rat) ")")
)

(println "Result:" (show-rational (calculate (parse-infix (tokenise inp)))))
