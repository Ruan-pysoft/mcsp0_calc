; run this file with ./rho calc.lisp
; a calculator program for the first "Monthly" Computer Science Project

(def .calc.libncurses (!ffi-load "libncursesw.so"))
(def .calc.libc (!ffi-load "libc.so.6"))

(if (= (type .calc.libncurses) ' string) (do
  (write stderr (&$ "Couldn't load ncurses: " .calc.libncurses # \n))
  (exit 1)
))

(def int ' i32)
(def i64 ' i64)
(def u64 ' u64)
(def void nil)
(def ptr ' ptr)

(defn nc.call (name ret () args)
  (call !ffi-call (cons
    (!ffi-sym .calc.libncurses name)
    (cons ret args)
  ))
)
(defn libc.call (name ret () args)
  (call !ffi-call (cons
    (!ffi-sym .calc.libc name)
    (cons ret args)
  ))
)
(defn nc.var (name)
  (list ' pointer (scd (!ffi-sym .calc.libncurses name)))
)
(defn libc.var (name)
  (list ' pointer (scd (!ffi-sym .calc.libc name)))
)

(defn read-var (type var)
  (head (!destruct-val var type))
)

(defn chain-cons (fst () rest)
  (if rest
    (cons fst (call chain-cons rest))
    fst
  )
)

; ncurses functions

(defn nc.initscr ()
  (nc.call "initscr" ptr)
)
(defn nc.endwin ()
  (nc.call "endwin" int)
)

(defn nc.printw (fmt () args)
  "each argument must be preceeded by a type!"
  (call nc.call (chain-cons
    "printw" int
    ptr (!string-data-pointer fmt)
    args
  ))
)

(defn nc.refresh ()
  (nc.call "refresh" int)
)

(defn nc.getch ()
  (nc.call "getch" int)
)

; libc functions

(defn libc.malloc ()
  (libc.call "malloc" ptr u64)
)
(defn libc.free ()
  (libc.call "free" void ptr)
)

; main program
(def stdscr (nc.var "stdscr"))

(nc.initscr)
(nc.printw "Hello World !!!")
(nc.refresh)
(nc.getch)
(nc.endwin)
