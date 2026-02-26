(if (not utils-header-guard) (do

  (def utils-header-guard T)

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

  (defn join (l1 l2)
    (if l2
      (join (append l1 (head l2)) (tail l2))
      l1
    )
  )

  (defn skip$ (s n) (if (>= (len$ s) n) ([]$ s n (len$ s)) ""))
  (defn tail$ (s) (skip$ s 1))

))
