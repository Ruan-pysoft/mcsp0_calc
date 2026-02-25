; run this file with ./rho calc.lisp
; a calculator program for the first "Monthly" Computer Science Project

(include "ncurses.lisp")

; main program
(nc.initscr)

(nc.cbreak)
(nc.noecho)
(nc.keypad (stdscr) T)

(nc.printw "Hello World !!!")

(def ch0 0)
(def ch1 0)
(def ch2 0)

(defn shiftch (newch) (do
  (:= ch0 ch1)
  (:= ch1 ch2)
  (:= ch2 newch)
))

(tmpfn ()
  (do
    (assoc (
      stringify (\ (ch)
        (if (< ch 256)
          (&$ (repr (&$ ch)) 0)
          (&$ "n" (repr ch) 0)
        )
      )
      s0 (stringify ch0)
      s1 (stringify ch1)
      s2 (stringify ch2)
    ) (do
      (nc.mvprintw 3 3 "Got input: %s (%d)     " ptr (!string-data-pointer s0) int ch0)
      (nc.mvprintw 4 3 "Got input: %s (%d)     " ptr (!string-data-pointer s1) int ch1)
      (nc.mvprintw 5 3 "Got input: %s (%d)     " ptr (!string-data-pointer s2) int ch2)
    ))

    (nc.move 0 0)

    (nc.refresh)

    (shiftch (nc.getch))

    (if (= ch2 # q) () (rec))
  )
  ()
)

(nc.endwin)
