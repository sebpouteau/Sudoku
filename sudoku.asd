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
   (:file "package")
   (:file "sudoku-static-method")
   (:file "sudoku-generic")
   (:file "sudoku-method")
   (:file "sudoku-affichage")
   (:file "strategy-intelligent")
   (:file "sudoku-strategy")
   (:file "coor")
   ))
