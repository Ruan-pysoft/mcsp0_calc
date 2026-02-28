(if (not state-header-guard) (do
  (def state-header-guard T)

  (def syms '(
    "(" ")"
  ))

  (def unary-ops (list
    (list "sinh" "TODO: sinh")
    (list "sin" "TODO: sin") (list "cos" "TODO: cos")
  ))

  (def unary-or-binary-ops (list
    (list "-" 1 -) (list "+" 1 +)
  ))

  (def binary-ops (list
    (list "*" 2 *) (list "/" 2 /) (list "%" 2 /)
    (list "^" 3 ^)
  ))

  (def functions ())
  (def variables (list
    (list "TAU" 6)
  ))

  (defn add-element (collection element) (tmpfn (c-back c e)
    (if (and c (< (len$ (head e)) (len$ (head (head c)))))
      (rec
        (append c-back (head c))
        (tail c)
        e
      )
      (join c-back (cons e c))
    )
    (() collection element)
  ))
  (defm add-element-to (collection element)
    (:= collection (add-element collection element))
  )

  (def precedence-levels '(1 2 3))

  (defn add-precedence-level (n)
    (:= precedence-levels (tmpfn (p-back p n)
      (cond
        ((and p (< (head p) n))
          (rec
            (append p-back (head p))
            (tail p)
            n
          )
        )
        ((and p (= (head p) n))
          (join p-back p)
        )
        (T (join p-back (cons n p)))
      )
      (nil precedence-levels n)
    ))
  )

  (defn add-unary-op (name op)
    (add-element-to unary-ops (list name op))
  )
  (defn add-unary-or-binary-op (name precedence op) (do
    (add-element-to unary-or-binary-ops (list name precedence op))
    (add-precedence-level precedence)
  ))
  (defn add-binary-op (name precedence op) (do
    (add-element-to binary-ops (list name precedence op))
    (add-precedence-level precedence)
  ))
  (defn add-function (name arguments op)
    (add-element-to functions (list name arguments op))
  )
  (defn add-variable (name val)
    (add-element-to variables (list name val))
  )

  ((\* (().) ())
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
  )

))
