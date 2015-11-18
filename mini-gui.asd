(cl:in-package #:common-lisp-user)

(asdf:defsystem :mini-gui
  :depends-on (:hunchentoot :sudoku :cl-who)
  :serial t
  :components
  ((:file "sudoku-prof/package-gui")
   (:file "sudoku-prof/mini-gui")
   ))
