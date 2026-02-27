(if (not state-header-guard) (do
  (def state-header-guard T)

  (def syms '(
    "(" ")"
  ))

  (def unary-ops (list
    (list "sinh" nil)
    (list "sin" nil) (list "cos" nil)
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
