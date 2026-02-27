(include "utils.lisp")

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
    (skip-while$ (tail$ text) predicate)
    text
  )
)

(defn starts-with$? (text begin)
  (and
    (>= (len$ text) (len$ begin))
    (= ([]$ text 0 (len$ begin)) begin)
  )
)

(def syms '(
  "sinh" "cosh"
  "sin" "cos"
  "+" "-" "*" "/" "%" "^"
  "(" ")"
))

(defn add-sym (sym)
  (:= syms (tmpfn (syms-back syms sym)
    (if (and syms (< (len$ sym) (len$ (head syms))))
      (rec
        (append syms-back (head syms))
        (tail syms)
        sym
      )
      (join syms-back (cons sym syms))
    )
    (() syms sym)
  ))
)

(defn match-sym (text syms)
  (if syms
    (if (starts-with$? text (head syms))
      (head syms)
      (match-sym text (tail syms))
    )
  )
)

(defn has-num? (text) (check$ text digit$?))

(defn has-space? (text) (check$ text space$?))

(defn has-tok? (text) (or
  (has-num? text)
  (truthy? (match-sym text syms))
  (has-space? text)
))

(defn get-num (text) (tmpfn (acc text)
  (if (check$ text digit$?)
    (rec
      (+ (* acc 10) (- ([]$ text 0) # 0))
      (tail$ text)
    )
      (list (list ' number acc) text)
  )
  (0 text)
))

(defn get-space (text)
  (if (check$ text space$?)
    (get-space (tail$ text))
    (list '(space) text)
  )
)

(defn get-undef (text) (tmpfn (acc text)
  (if (and text (not (has-tok? text)))
    (rec (&$ acc ([]$ text 0)) (tail$ text))
    (list (list ' undefined acc) text)
  )
  ("" text)
))

(defn get-tok (text)
  (cond
    ((not text) ())
    ((has-num? text) (get-num text))
    ((match-sym text syms) (assoc (sym (match-sym text syms))
      (list (list ' symbol sym) (skip$ text (len$ sym)))
    ))
    ((has-space? text) (get-space text))
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
