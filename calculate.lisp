(include "utils.lisp")
(include "state.lisp")

(defn map (fn lst)
  (if lst
    (cons (fn (head lst)) (map fn (tail lst)))
  )
)

(defn add (a () b)
  (if b
    (assoc (b (head b))
      (if (= (nth a 2) (nth b 2))
        (list ' over (+ (scd a) (scd b)) (nth a 2))
        (add
          (list ' over (* (scd a) (nth b 2)) (* (nth a 2) (nth b 2)))
          (list ' over (* (scd b) (nth a 2)) (* (nth a 2) (nth b 2)))
        )
      )
    )
    a
  )
)

(defn sub (a () b)
  (if b
    (add a (sub (head b)))
    (list ' over (- (scd a)) (nth a 2))
  )
)

(defn call.function (fn args)
  (cond
    ((= fn "+") (call add args))
    ((= fn "-") (call sub args))
    ;((= fn "*") (call * args))
    ;((= fn "juxtapose") (call * args))
    ;((= fn "/") (call / args))
    ;((= fn "**") (call ** args))
    ;((= fn "%") (call % args))
    (T (println "unrecognised function" fn) (exit 1))
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
    ((and (= (type expr) ' list) (= (head expr) ' over))
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
