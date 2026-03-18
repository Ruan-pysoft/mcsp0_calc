(include "utils.lisp")
(include "state.lisp")

(defn calculate (expr)
  (cond
    ((= (type expr) ' number)
      number
    )
    ((= (type expr) ' list)
      (println "calculating expression:" expr)
      0
    )
    (T
      (println "something went wrong! D:")
      (exit 1)
    )
  )
)
