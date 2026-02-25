(def .ncurses.libncurses (!ffi-load "libncursesw.so"))
(def .ncurses.libc (!ffi-load "libc.so.6"))

(if (= (type .ncurses.libncurses) ' string) (do
  (write stderr (&$ "Couldn't load ncurses: " .ncurses.libncurses # \n))
  (exit 1)
))

(def u8 ' u8)
(def int ' i32)
(def i64 ' i64)
(def u64 ' u64)
(def void nil)
(def ptr ' ptr)
(def chtype ' u32)

(defn nc.call (name ret () args)
  (call !ffi-call (cons
    (!ffi-sym .ncurses.libncurses name)
    (cons ret args)
  ))
)
(defn libc.call (name ret () args)
  (call !ffi-call (cons
    (!ffi-sym .ncurses.libc name)
    (cons ret args)
  ))
)
(defn nc.var (name)
  (list ' pointer (scd (!ffi-sym .ncurses.libncurses name)))
)
(defn libc.var (name)
  (list ' pointer (scd (!ffi-sym .ncurses.libc name)))
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

(defn nc.getch ()
  (nc.call "getch" int)
)

(defn nc.cbreak ()
  (nc.call "cbreak" int)
)

(defn nc.echo ()
  (nc.call "echo" int)
)
(defn nc.noecho ()
  (nc.call "noecho" int)
)
(defn nc.echo? ()
  (truthy? (nc.call "is_echo" int))
)

(defn nc.keypad (win bool)
  (nc.call "keypad" int ptr win int (if bool 1 0))
)

(defn nc.move (y x)
  (nc.call "move" int int y int x)
)
(defn nc.wmove (win y x)
  (nc.call "move" int ptr win int y int x)
)

(defn nc.addch (ch)
  (nc.call "addch" int chtype ch)
)
(defn nc.waddch (win ch)
  (nc.call "waddch" int ptr win chtype ch)
)
(defn nc.mvaddch (y x ch)
  (nc.call "addch" int int y int x chtype ch)
)
(defn nc.mvwaddch (win y x ch)
  (nc.call "waddch" int ptr win int y int x chtype ch)
)

(defn nc.printw (fmt () args)
  "each argument must be preceeded by a type!"
  (assoc (fmt (&$ fmt 0))
    (call nc.call (chain-cons
      "printw" int
      ptr (!string-data-pointer fmt)
      args
    ))
  )
)
(defn nc.wprintw (win fmt () args)
  "each argument must be preceeded by a type!"
  (assoc (fmt (&$ fmt 0))
    (call nc.call (chain-cons
      "wprintw" int
      ptr win
      ptr (!string-data-pointer fmt)
      args
    ))
  )
)
(defn nc.mvprintw (y x fmt () args)
  "each argument must be preceeded by a type!"
  (assoc (fmt (&$ fmt 0))
    (call nc.call (chain-cons
      "mvprintw" int
      int y int x
      ptr (!string-data-pointer fmt)
      args
    ))
  )
)
(defn nc.mvwprintw (win y x fmt () args)
  "each argument must be preceeded by a type!"
  (assoc (fmt (&$ fmt 0))
    (call nc.call (chain-cons
      "mvprintw" int
      ptr win int y int x
      ptr (!string-data-pointer fmt)
      args
    ))
  )
)

(defn .nc.addstr (str)
  (assoc (str (&$ str 0))
    (nc.call "addstr" int
      ptr (!string-data-pointer str)
    )
  )
)
(defn .nc.mvaddstr (y x str)
  (assoc (str (&$ str 0))
    (nc.call "mvaddstr" int
      int y int x
      ptr (!string-data-pointer str)
    )
  )
)
(defn .nc.waddstr (win str)
  (assoc (str (&$ str 0))
    (nc.call "waddstr" int
      ptr win
      ptr (!string-data-pointer str)
    )
  )
)
(defn .nc.mvwaddstr (win y x str)
  (assoc (str (&$ str 0))
    (nc.call "mvwaddstr" int
      ptr win int y int x
      ptr (!string-data-pointer str)
    )
  )
)

(defn nc.addnstr (str n)
  (nc.call "addnstr" int
    ptr (!string-data-pointer str)
    int n
  )
)
(defn nc.mvaddnstr (y x str n)
  (nc.call "mvaddnstr" int
    int y int x
    ptr (!string-data-pointer str)
    int n
  )
)
(defn nc.waddnstr (win str n)
  (nc.call "waddnstr" int
    ptr win
    ptr (!string-data-pointer str)
    int n
  )
)
(defn nc.mvwaddnstr (win y x str n)
  (nc.call "mvwaddnstr" int
    ptr win int y int x
    ptr (!string-data-pointer str)
  )
)

(defn nc.addstr (str) (nc.addnstr str (len$ str)))
(defn nc.mvaddstr (y x str) (nc.mvaddnstr y x str (len$ str)))
(defn nc.waddstr (win str) (nc.addnstr win str (len$ str)))
(defn nc.mvwaddstr (win y x str) (nc.mvaddnstr win y x str (len$ str)))

(defn nc.refresh ()
  (nc.call "refresh" int)
)

(defn nc.getcury (win)
  (nc.call "getcury" int ptr win)
)
(defn nc.getcurx (win)
  (nc.call "getcurx" int ptr win)
)
(defn nc.getbegy (win)
  (nc.call "getbegy" int ptr win)
)
(defn nc.getbegx (win)
  (nc.call "getbegx" int ptr win)
)
(defn nc.getmaxy (win)
  (nc.call "getmaxy" int ptr win)
)
(defn nc.getmaxx (win)
  (nc.call "getmaxx" int ptr win)
)
(defn nc.getpary (win)
  (nc.call "getpary" int ptr win)
)
(defn nc.getparx (win)
  (nc.call "getparx" int ptr win)
)

(defm nc.getyx (win y x)
  (do
    (:= y (nc.getcury win))
    (:= x (nc.getcurx win))
    nil
  )
)
(defm nc.getbegyx (win y x)
  (do
    (:= y (nc.getbegy win))
    (:= x (nc.getbegx win))
    nil
  )
)
(defm nc.getmaxyx (win y x)
  (do
    (:= y (nc.getmaxy win))
    (:= x (nc.getmaxx win))
    nil
  )
)
(defm nc.getparyx (win y x)
  (do
    (:= y (nc.getpary win))
    (:= x (nc.getparx win))
    nil
  )
)

(defn getch ()
  (nc.call "getch" int)
)
(defn wgetch (win)
  (nc.call "wgetch" int ptr win)
)
(defn mvgetch (y x)
  (nc.call "mvgetch" int int y int x)
)
(defn mvwgetch (y x win)
  (nc.call "mvwgetch" int int y int x ptr win)
)
(defn ungetch (c)
  (nc.call "ungetch" int int c)
)

; TODO: scanw?

(defn nc.wgetnstr (win n)
  (assoc (
    buf (libc.malloc (+ n 1))
    res ()
  ) (do
    (nc.call "wgetnstr" int ptr win ptr buf int n)
    (:= res (tmpfn (res buf)
      (if (!= (head (!destruct-val buf u8)) 0)
        (rec
          (&$ res (head (!destruct-val buf u8)))
          (list ' pointer (+ (scd buf) 1))
        )
        res
      )
      ("" buf)
    ))
    (libc.free buf)
    res
  ))
)
(defn nc.getnstr (n) (nc.wgetnstr (stdscr) n))

(defn nc.wgetstr (win) (tmpfn (acc)
  (assoc (ch (wgetch win))
    (switch ch
      (case (- 1) acc)
      (case # \n acc)
      (case # \r acc)
      (case 263 (if (len$ acc) ; backspace
        (do
          (if (nc.echo?) (do
            (nc.waddch win #sp)
            (nc.wmove win (nc.getcury win) (- (nc.getcurx win) 2))
          ))
          (rec ([]$ acc 0 (- (len$ acc) 1)))
        )
        (rec acc)
      )) ; backspace
      (default (if (and (>= ch 0) (< ch 256))
        (rec (&$ acc ch))
        (rec acc)
      ))
    )
  )
  ("")
))
(defn nc.getstr () (nc.wgetstr (stdscr)))

(def .ncurses.stdscr (nc.var "stdscr"))
(defn stdscr () (read-var ptr .ncurses.stdscr))

; libc functions

(defn libc.malloc (size)
  (libc.call "malloc" ptr u64 size)
)
(defn libc.free (thing)
  (libc.call "free" void ptr thing)
)
