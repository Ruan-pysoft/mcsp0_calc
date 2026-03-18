(include "utils.lisp")
(include "state.lisp")

(defn map (fn list)
  (if list
    (cons (fn (head list)) (map fn (tail list)))
  )
)

(defn call.function (fn args)
  (cond
    ((= fn "+") (call + args))
    ((= fn "-") (call - args))
    ((= fn "*") (call * args))
    ((= fn "juxtapose") (call * args))
    ((= fn "/") (call / args))
    ((= fn "**") (call ** args))
    ((= fn "%") (call % args))
  )
)

(defn fetch.variable (var)
  (tmpfn (var vars)
    (if vars
      (if (= var (head (head vars)))
        (scd (head vars))
        (rec var (tail vars))
      )
      (do
        (println (&$ "Undefined variable " var "!"))
        (exit 1)
      )
    )
    (var variables)
  )
)

(defn calculate (expr)
  (cond
    ((= (type expr) ' number)
      expr
    )
    ((= (type expr) ' list)
      (call.function (head expr) (map calculate (tail expr)))
    )
    ((= (type expr) ' string)
      (fetch.variable expr)
    )
    (T
      (println "something went wrong! D:")
      (exit 1)
    )
  )
)
