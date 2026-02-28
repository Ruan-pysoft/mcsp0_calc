; simple recursive descent parser

(include "utils.lisp")

(defn tt? (token typ)
  (and
    token
    (= (head token) typ)
  )
)

(defn tv (token) (scd token))

(defn check-next? (tokens token) (or
  (and tokens (= (head tokens) token))
  (and (>= (len tokens) 2) (tt? (head tokens) ' space) (= (scd tokens) token))
))
(defn check-next-tt? (tokens typ) (or
  (and tokens (= (head (head tokens)) typ))
  (and (>= (len tokens) 2) (tt? (head tokens) ' space) (= (head (scd tokens)) typ))
))
(defn skip-tok (tokens)
  (if tokens
    (if (tt? (head tokens) ' space)
      (tail (tail tokens))
      (tail tokens)
    )
  )
)
(defn fetch-tok (tokens)
  (if tokens
    (if (tt? (head tokens) ' space)
      (if (tail tokens) (scd tokens))
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

(defn parse-binary (tokens prec precs)
  ;(do (println "parsing binary expression with precedence" prec "from:" tokens)
  (assoc (rec (\ (lhs tokens) (cond
    ;((println "got lhs" lhs "and toks" tokens "@ prec" prec nil) nil)
    ((= tokens nil) (list lhs nil))
    ((and
      ;(println "checking token type of" (fetch-tok tokens) T)
      (or (tt? (fetch-tok tokens) ' op-b) (tt? (fetch-tok tokens) ' op-ub))
      ;(println "checking token precedence..." T)
      (= (nth (fetch-tok tokens) 2) prec)
    ) ; got operator of the right precedence!
      ;(println "matched token")
      ;(println "Matched token" (fetch-tok tokens) "with precedence level" precs)
      ;(println "Remaining tokens:" (skip-tok tokens))
      (assoc
        (op (fetch-tok tokens)
         res (parse-binary (skip-tok tokens) (head precs) (tail precs))
         val (head res)
         tokens (scd res)
        )
        (rec (list (nth op 3) lhs val) tokens)
      )
    )
    (T (list lhs tokens))
  )))
    (if precs
      (call rec (parse-binary tokens (head precs) (tail precs)))
      (call rec (parse-base tokens))
    )
  )
  ;)
)

(defn parse-expression (tokens)
  (parse-binary tokens (head precedence-levels) (tail precedence-levels))
)

(defn parse-infix (tokens)
  (head (parse-expression tokens))
)
