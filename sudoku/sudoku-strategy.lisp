
;;===========================================
;;==           Implémentation              ==
;;==            FONCTIONS IA               ==
;;===========================================


(in-package :sudoku)

(defmethod init-standalone (tab)
  (setf *game* (make-instance 'game
			      :game-squares (make-squares)
			      :initial-grid tab))
  ;(setf (initial-grid *game*) tab)
  (init-game *game*)
  (update-possibility-all-square (game-squares *game*))
  )


(defmethod main-standalone ()
  (multiple-value-bind (x y value) (strategy *game*)
    (when x
      (change-digit *game* x y value)
      (update-after-change-digit *game* x y)
      (values x y value)))
  )
