(cl:in-package #:common-lisp-user)

(defpackage :sudoku-system
  (:use :asdf :common-lisp))

(in-package :sudoku-system)

;;(defparameter *sudoku-directory* (directory-namestring *load-truename*))
;;(format t "sudoku-directory is ~A ~%" *sudoku-directory*)

(asdf:defsystem :sudoku
  :serial t
  :components
  (
   (:file "sudoku/package")
   (:file "sudoku/sudoku-static-method")
   (:file "sudoku/sudoku-generic")
   (:file "sudoku/sudoku-method")
   (:file "sudoku/sudoku-affichage")
   (:file "strategy/strategy-smart")
   (:file "sudoku/sudoku-strategy")
   (:file "sudoku-prof/coor")
   ))

