(in-package :sudoku)


(defun list-to-2d-array (list)
  (make-array (list (length list)
                    (length (first list)))
              :initial-contents list))

(defun line-column (sens x y)
  "renvoie la liste '(x y) ou '(y x) si l'on traite les column ou les lignes"
  (if (eq sens 'line)
      (list y x)
      (list x y)))

(defun init-sudoku ()
  )
