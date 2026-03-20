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
(defn identifier-like? (token) (or
  (tt? token ' undefined)
  (tt? token ' symbol)
  (tt? token ' op-u)
  (tt? token ' op-ub)
  (tt? token ' op-b)
  (tt? token ' fn)
  (tt? token ' var)
))

(defn parse-base (tokens)
  (cond
    ((check-next-tt? tokens ' number)
      (list (list ' over (tv (fetch-tok tokens)) 1) (skip-tok tokens))
    )
    ((check-next-tt? tokens ' var)
      (list (scd (fetch-tok tokens)) (skip-tok tokens))
    )
    ((check-next? tokens '(symbol "("))
      (assoc (expr (parse-expression (skip-tok tokens)) val (head expr) tokens (scd expr))
        (cond
          ((check-next? tokens '(symbol ")"))
            (list val (skip-tok tokens))
          )
          (T
            (write stderr (&$ "ERROR: expected matching closing paren @ " (repr tokens) # \n))
            (exit 1)
            ; TODO: proper error handling & reporting
          )
        )
      )
    )
    (T
      (write stderr (&$ "Unexpected tokens: " (repr tokens) # \n))
      (exit 1)
    )
  )
)

(defn parse-juxtaposition (tokens)
  (assoc (rec (\ (lhs tokens) (cond
    ; ((println "juxtaposition with lhs" lhs "and tokens" tokens nil) nil)
    ((= tokens nil) (list lhs nil))
    ((or
      (check-next-tt? tokens ' number)
      (check-next-tt? tokens ' var)
      (check-next? tokens '(symbol "("))
    ) (assoc (res (parse-base tokens) val (head res) tokens (scd res))
      (rec (list "juxtapose" lhs val) tokens)
    ))
    ((check-next-tt? tokens ' op-u) (assoc (res (parse-unary tokens) val (head res) tokens (scd res))
      (rec (list "juxtapose" lhs val) tokens)
    ))
    (T (list lhs tokens))
  )))
    (call rec (parse-base tokens))
  )
)

(defn parse-unary (tokens)
  (cond
    ((= tokens nil)
      (write stderr "ERROR: Unexpected EOF when parsing unary expression\n")
      (exit 1)
    )
    ((or (tt? (fetch-tok tokens) ' op-u) (tt? (fetch-tok tokens) ' op-ub))
      (assoc
        (op (fetch-tok tokens)
         res (parse-unary (skip-tok tokens))
         val (head res)
         tokens (scd res)
        )
        (if (tt? op ' op-u)
          (list (list (nth op 2) val) tokens)
          (list (list (nth op 3) val) tokens)
        )
      )
    )
    (T (parse-juxtaposition tokens))
  )
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
         res (if precs
           (parse-binary (skip-tok tokens) (head precs) (tail precs))
           (parse-unary (skip-tok tokens))
         )
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
      (call rec (parse-unary tokens))
    )
  )
  ;)
)

(defn parse-expression (tokens)
  (parse-binary tokens (head precedence-levels) (tail precedence-levels))
)

(defn parse-let (tokens)
  (do
    (assert (= (head tokens) '(keyword-let)))
    (:= tokens (tail tokens))
    (if (identifier-like? (fetch-tok tokens))
      (assoc (name (scd (fetch-tok tokens)) tokens (skip-tok tokens)) (do
          (if (or (not (identifier-like? (fetch-tok tokens))) (!= (scd (fetch-tok tokens)) "=")) (do
            (write stderr (&$ "ERROR: in let, expected =, but got an unexpected token " (repr tokens) # \n))
            (exit 1)
          ))
          (assoc (expr-and-toks (parse-expression (skip-tok tokens)) expr (head expr-and-toks) tokens (scd expr-and-toks))
            (list (list ' let name expr) tokens)
          )
      ))
      (do
        (write stderr (&$ "ERROR: in let, expected an identifier, but got an unexpected token " (repr tokens) # \n))
        (exit 1)
      )
    )
  )
)

(defn parse-stmt (tokens)
  (cond
    ((and tokens (= (head tokens) '(keyword-let))) (parse-let tokens))
    (T (parse-expression tokens))
  )
)

(defn parse-infix (tokens)
  (head (parse-stmt tokens))
)
