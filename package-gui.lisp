(cl:in-package :common-lisp-user)

(defpackage :gui-sudoku
  (:use :common-lisp :sudoku :cl-who :hunchentoot)
  (:export :sudoku))

