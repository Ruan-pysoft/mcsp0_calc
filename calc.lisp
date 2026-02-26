; run this file with ./rho calc.lisp
; a calculator program for the first "Monthly" Computer Science Project

; See: https://en.wikipedia.org/wiki/Operator-precedence_parser

(defn quote-each (vals) (tmpfn (acc vals)
  (if vals
    (rec
      (append acc (list ' quote (head vals)))
      (tail vals)
    )
    acc
  )
  (nil vals)
))

(defm cond (() conditions)
  (call tmpfn (list
    '(.conds)
    '(if .conds (do
      (assert (= (type (head .conds)) ' list))
      (assert (!= (head .conds) nil))
      (if (eval (head (head .conds)))
        (call do (tail (head .conds)))
        (rec (tail .conds))
      )
    ))
    '(' conditions)
  ))
)

(defn check-n$ (text n predicate)
  (and
    (> (len$ text) n)
    (predicate ([]$ text n))
  )
)
(defn check$ (text predicate) (check-n$ text 0 predicate))

(defn space$? (ch) (<= ch #sp))
(defn digit$? (ch) (and (>= ch # 0) (<= ch # 9)))

(defn skip-while$ (text predicate)
  (if (check$ text predicate)
    (skip-while$ ([]$ text 1 (len$ text)) predicate)
    text
  )
)

(defn starts-with$? (text begin)
  (and
    (>= (len$ text) (len$ begin))
    (= ([]$ text 0 (len$ begin)) begin)
  )
)

(defn join (l1 l2)
  (if l2
    (join (append l1 (head l2)) (tail l2))
    l1
  )
)

(def syms '(
  "sinh" "cosh"
  "sin" "cos"
  "+" "-" "*" "/" "%" "^"
))

(defn match-sym (text syms)
  (if syms
    (if (starts-with$? text (head syms))
      (head syms)
      (match-sym text (tail syms))
    )
  )
)

(defm =? (to) (\ (x) (= x to)))

(defn has-num? (text) (or
  (check$ text digit$?)
  (and (check$ text (=? # -)) (check-n$ text 1 digit$?))
))

(defn has-tok? (text) (or
  (has-num? text)
  (truthy? (match-sym text syms))
))

(defn get-num (text)
  (assoc (rec (\ (acc text)
    (if (check$ text digit$?)
      (rec
        (+ (* acc 10) (- ([]$ text 0) # 0))
        ([]$ text 1 (len$ text))
      )
      (list (list ' number acc) text)
    )
  ))
    (if (check$ text (=? # -))
      (assoc (res (rec 0 ([]$ text 1 (len$ text))))
        (list
          (list ' number (- (scd (head res))))
          (scd res)
        )
      )
      (rec 0 text)
    )
  )
)

(defn get-undef (text) (tmpfn (acc text)
  (if (and text (not (has-tok? text)))
    (rec (&$ acc ([]$ text 0)) ([]$ text 1 (len$ text)))
    (list (list ' undefined acc) text)
  )
  ("" text)
))

(defn get-tok (text)
  (cond
    ((not text) ())
    ((has-num? text) (get-num text))
    ((match-sym text syms) (assoc (sym (match-sym text syms))
      (list sym ([]$ text (len$ sym) (len$ text)))
    ))
    (T (get-undef text))
  )
)

(defn tokenise (text) (tmpfn (acc text)
  (if text
    (assoc (tok-and-text (get-tok text))
      (rec
        (append acc (head tok-and-text))
        (scd tok-and-text)
      )
    )
    acc
  )
  (nil text)
))

(def inp (readline stdin))

(println "Got input:" inp)
(println "Tokenised input:" (tokenise inp))
