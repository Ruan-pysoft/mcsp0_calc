; simple recursive descent parser

(include "utils.lisp")

(defm tt? (token typ)
  (= (head token) ' typ)
)

(defn tv (token) (scd token))

(defn check-next? (tokens token) (or
  (and tokens (= (head tokens) token))
  (and (>= (len tokens) 2) (tt? (head tokens) space) (= (scd tokens) token))
))
(defn check-next-tt? (tokens typ) (or
  (and tokens (= (head (head tokens)) typ))
  (and (>= (len tokens) 2) (tt? (head tokens) space) (= (head (scd tokens)) typ))
))
(defn skip-tok (tokens)
  (if tokens
    (if (tt? (head tokens) space)
      (tail (tail tokens))
      (tail tokens)
    )
  )
)
(defn fetch-tok (tokens)
  (if tokens
    (if (tt? (head tokens) space)
      (scd tokens)
      (head tokens)
    )
  )
)

(defn parse-base (tokens)
  (cond
    ((check-next-tt? tokens ' number)
      (list (tv (fetch-tok tokens)) (skip-tok tokens))
    )
    ((check-next? tokens '(symbol "("))
      (assoc (expr (parse-expression (skip-tok tokens)) val (head expr) tokens (scd expr))
        (cond
          ((check-next? tokens '(symbol ")"))
            (list val (skip-tok tokens))
          )
          (T
            (write stderr "ERROR: expected matching closing paren")
            (exit 1)
            ; TODO: proper error handling & reporting
          )
        )
      )
    )
  )
)

(defn parse-fact (tokens)
  (call tmpfn (list
    '(lhs tokens)
    '(cond
      ((= tokens nil) (list lhs nil))
      ((check-next? tokens '(symbol "*")) (assoc
        (res (parse-base (skip-tok tokens)) val (head res) tokens (scd res))
        (rec (list "*" lhs val) tokens)
      ))
      ((check-next? tokens '(symbol "/")) (assoc
        (res (parse-base (skip-tok tokens)) val (head res) tokens (scd res))
        (rec (list "/" lhs val) tokens)
      ))
      ((check-next? tokens '(symbol "%")) (assoc
        (res (parse-base (skip-tok tokens)) val (head res) tokens (scd res))
        (rec (list "%" lhs val) tokens)
      ))
      (T (list lhs tokens))
    )
    (quote-each (parse-base tokens))
  ))
)

(defn parse-term (tokens)
  (call tmpfn (list
    '(lhs tokens)
    '(cond
      ((= tokens nil) (list lhs nil))
      ((check-next? tokens '(symbol "+")) (assoc
        (res (parse-fact (skip-tok tokens)) val (head res) tokens (scd res))
        (rec (list "+" lhs val) tokens)
      ))
      ((check-next? tokens '(symbol "-")) (assoc
        (res (parse-fact (skip-tok tokens)) val (head res) tokens (scd res))
        (rec (list "-" lhs val) tokens)
      ))
      (T (list lhs tokens))
    )
    (quote-each (parse-fact tokens))
  ))
)

(defn parse-expression (tokens)
  (parse-term tokens)
)

(defn parse-infix (tokens)
  (head (parse-expression tokens))
)
