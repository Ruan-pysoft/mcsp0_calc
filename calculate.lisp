(include "utils.lisp")
(include "state.lisp")

(defn map (fn lst)
  (if lst
    (cons (fn (head lst)) (map fn (tail lst)))
  )
)

(defn simplify (n)
  (if (< (nth n 2) 0)
    (simplify (list ' over (- (scd n)) (- (nth n 2))))
    (assoc (fact (gcd (scd n) (nth n 2)))
      (list ' over (/ (scd n) fact) (/ (nth n 2) fact))
    )
  )
)

(defn add (a () b)
  (if b
    (assoc (b (head b))
      (if (= (nth a 2) (nth b 2))
        (simplify (list ' over (+ (scd a) (scd b)) (nth a 2)))
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

(defn gcd (a b)
  (cond
    ((or (= a 1) (= b 1)) 1)
    ((< a 0) (gcd (- a) b))
    ((< b 0) (gcd a (- b)))
    ((> a b) (gcd b a))
    ((= a b) a)
    ((= a 0) b)
    (T (gcd (% b a) a))
  )
)

(defn gcd.op (a b)
  (if (or (!= (nth a 2) 1) (!= (nth a 2) 1))
    (do (println "gcd is not defined on non-integers!") (exit 1))
    (list ' over (gcd (scd a) (scd b)) 1)
  )
)

(defn multiply (a b)
  (simplify
    (list ' over (* (scd a) (scd b)) (* (nth a 2) (nth b 2)))
  )
)

(defn divide (a b)
  (simplify
    (list ' over (* (scd a) (nth b 2)) (* (nth a 2) (scd b)))
  )
)

; https://math.stackexchange.com/a/621014
(defn modulo (a b)
  (sub a (multiply b (floor (divide a b))))
)

(defn ipart (x)
  "round towards zero"
  (list ' over (/ (scd x) (nth x 2)) 1)
)

(defn fpart (x)
  (sub x (ipart x))
)

(defn floor (x)
  (if (< (scd x) 0)
    (assoc (n (ipart (list ' over (+ (scd x) 1) (nth x 2))))
      (list ' over (- (scd n) 1) 1)
    )
    (ipart x)
  )
)

(defn ceil (x)
  (if (< (scd x) 0)
    (ipart x)
    (assoc (n (ipart (list ' over (- (scd x) 1) (nth x 2))))
      (list ' over (+ (scd n) 1) 1)
    )
  )
)

(defn round (x)
  (floor (add x '(over 1 2)))
)

(defn call.function (fn args)
  (cond
    ((= fn "+") (call add args))
    ((= fn "-") (call sub args))
    ((= fn "gcd") (call gcd.op args))
    ((= fn "*") (call multiply args))
    ((= fn "juxtapose") (call multiply args))
    ((= fn "/") (call divide args))
    ;((= fn "**") (call ** args))
    ((= fn "%") (call modulo args))
    ((= fn "ipart") (call ipart args))
    ((= fn "fpart") (call fpart args))
    ((= fn "floor") (call floor args))
    ((= fn "ceil") (call ceil args))
    ((= fn "round") (call round args))
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
