; run this file with ./rho calc.lisp
; a calculator program for the first "Monthly" Computer Science Project

(include "ncurses.lisp")

; main program
(nc.initscr)

(nc.cbreak)
(nc.noecho)
(nc.keypad (nc.stdscr) T)

(defn new-win (height width starty startx) (assoc
  (local-win (nc.newwin height width starty startx)) (do
    (println "created window with dimensions" width "x" height "at" startx "," starty ":" local-win)
    (nc.box local-win 0 0)
    (nc.border # a # b # c # d # e # f # g # h)
    (nc.wrefresh local-win)
    local-win
  )
))

(defn destroy-win (local-win)
  (do
    (nc.wborder local-win #sp #sp #sp #sp #sp #sp #sp #sp)
    (nc.wrefresh local-win)
    (nc.refresh)
    (nc.delwin local-win)
  )
)

(def my-win nil)
(def startx 0)
(def starty 0)
(def height 3)
(def width 10)
(def ch 0)

(:= starty (/ (- (nc.LINES) height) 2))
(:= starty (/ (- (nc.COLS) width) 2))

(nc.printw "Press F1 to exit")
(nc.refresh)

(:= my-win (new-win height width starty startx))

(tmpfn ()
  (if (!= (:= ch (nc.getch)) 265) ; F1 key
    (do
      (switch ch
        (case 260 (do ; left arrow
          (destroy-win my-win)
          (:= my-win (new-win height width starty (-- startx)))
        ))
        (case 261 (do ; right arrow
          (destroy-win my-win)
          (:= my-win (new-win height width starty (++ startx)))
        ))
        (case 259 (do ; up arrow
          (destroy-win my-win)
          (:= my-win (new-win height width (-- starty) startx))
        ))
        (case 258 (do ; down arrow
          (destroy-win my-win)
          (:= my-win (new-win height width (++ starty) startx))
        ))
      )
      (rec)
    )
  )
  ()
)

(nc.endwin)
