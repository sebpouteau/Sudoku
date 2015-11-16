(defpackage :sudoku
  (:use :common-lisp)
   (:export
   #:*game*
   #:*sqrt-size*
   #:coor
   #:square
   #:squares
   #:game
   #:game-over
   #:game-with-grid
   #:init-game
   #:coor-square
   #:make-square
   #:make-coor
   #:game-squares
   #:digit
   #:next-move
   #:possible-digits
   #:rcoor-to-coor
   #:zcoor-to-zone
   #:game-do
   #:assigned-p
   #:x-coor
   #:y-coor
   #:protected
   #:run-game
   #:init-sudoku
   #:game-with-new-grid
   ))

(in-package :sudoku)
