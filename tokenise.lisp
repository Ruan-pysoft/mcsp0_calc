(include "utils.lisp")
(include "state.lisp")

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

(defn match-collection (text collection)
  (if collection
    (if (starts-with$? text (head (head collection)))
      (head collection)
      (match-collection text (tail collection))
    )
  )
)

(defn match-sym (text) (tmpfn (text syms)
  (if syms
    (if (starts-with$? text (head syms))
      (head syms)
      (rec text (tail syms))
    )
  )
  (text syms)
))

(defn has-num? (text) (check$ text digit$?))

(defn has-space? (text) (check$ text space$?))

(defn has-let? (text) (and
  (starts-with$? text "let")
  (>= (len$ text) 4)
  (space$? ([]$ text 3))
))

(defn has-tok? (text) (or
  (has-num? text)
  (truthy? (has-let? text))
  (truthy? (match-sym text))
  (truthy? (match-collection text unary-ops))
  (truthy? (match-collection text unary-or-binary-ops))
  (truthy? (match-collection text binary-ops))
  (truthy? (match-collection text functions))
  (truthy? (match-collection text variables))
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

(defn get-let (text) (list '(keyword-let) (skip$ text 3)))

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
    ((has-let? text) (get-let text))
    ((match-sym text) (assoc (sym (match-sym text))
      (list (list ' symbol sym) (skip$ text (len$ sym)))
    ))
    ((match-collection text unary-ops) (assoc (op (match-collection text unary-ops))
      (list (cons ' op-u op) (skip$ text (len$ (head op))))
    ))
    ((match-collection text unary-or-binary-ops) (assoc (op (match-collection text unary-or-binary-ops))
      (list (cons ' op-ub op) (skip$ text (len$ (head op))))
    ))
    ((match-collection text binary-ops) (assoc (op (match-collection text binary-ops))
      (list (cons ' op-b op) (skip$ text (len$ (head op))))
    ))
    ((match-collection text functions) (assoc (fn (match-collection text functions))
      (list (cons ' fn fn) (skip$ text (len$ (head fn))))
    ))
    ((match-collection text variables) (assoc (var (match-collection text variables))
      (list (cons ' var var) (skip$ text (len$ (head var))))
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
