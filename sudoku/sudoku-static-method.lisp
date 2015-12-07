
;;===========================================
;;==           Implémentation              ==
;;==     FONCTIONS STATIQUES SUDOKU        ==
;;===========================================

(in-package :sudoku)


(defun list-to-2d-array (list)
  "Créer un tableau 2D à partir d'une liste"
  (make-array (list (length list)
                    (length (first list)))
              :initial-contents list)
  )

(defun line-column (sens x y)
  "Renvoie la liste '(x y) ou '(y x) selon si l'on traite les colonnes ou les lignes"
  (if (eq sens 'line)
      (list y x)
      (list x y))
  )

(defun init-sudoku ()
  "Méthode utilisée pour l'interface web"
  )
