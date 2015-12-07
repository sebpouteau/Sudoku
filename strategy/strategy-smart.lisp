;;=========================================
;;==           Implémentation            ==
;;==      FONCTIONS IA INTELLIGENTE      ==
;;=========================================

(in-package :sudoku)


;; -----------------------------
;; --   Fonctions statiques   --
;; -----------------------------

(defun strategy (game)
  ;; On essaye toutes les fonctions pour trouver un coup à jouer
  (substrategy game '(one-possibility-square
		      one-possibility-in-line
		      one-possibility-in-subsquare
		      depth-study-possibility))
  )

(defun substrategy (game list)
  "lance la stratégie avec les fonctions contenu dans la liste"
  (loop for fun in list do
       (multiple-value-bind (x y value)  (parcour-grid-with-function game fun)
	 (when x
	   (return-from substrategy (values x y value)))))
  )

(defun verification-square-valid (game x y)
  "Retourne si on peut jouer dans le square x y"
  (and (not (protected ( aref (squares-array (game-squares game)) x y)))
       (eq (digit (aref (squares-array (game-squares game)) x y)) 0))
)


(defun parcour-grid-with-function (game fun)
  " permet de parcourir la grille et d'appliquer une fonction à chaque square valide"
  (loop for y from 0 to (1- *size*) do
       (loop for x from 0 to (1- *size*) do
	    (when (verification-square-valid game x y)
	      (multiple-value-bind (x y value) (funcall fun game x y)
		;; si x est non nul, c'est qu'il y a une solution
		(when x
		  (return-from parcour-grid-with-function (values x y value)))))))
  )


(defun one-possibility-square (game x y)
  "Retourne x y value, d'un square n'ayant qu'une possibilité"
  (when (= 1 (length (get-possibility game x y)))
    (return-from one-possibility-square (values x y (car (get-possibility game x y)))))
  )


(defun one-possibility-in-subsquare (game x y)
  "Retourne x y value, s'il y a une possibilité unique dans un sous-square .
Par exemple si une value n'apparait que dans les possibilités d'un seul square du sous squares"
  (let ((list (verifier-subsquare game x y )))
    (when (= (length list) 1)
      (return-from one-possibility-in-subsquare (values x y (car list)))))
  )


(defun verifier-subsquare(game coorX coorY)
  "Vérifie unicité d'un nombre dans un sous squares"
  (let* ((list (get-possibility game coorX coorY))
	 ;; définition des débuts et fins du petit carré qui contient square (x,y)
	(departX (* (truncate (/ coorX *sqrt-size*)) *sqrt-size*))
	(finX (- (+ departX *sqrt-size*) 1)) 
	(departY (* (truncate (/ coorY *sqrt-size*)) *sqrt-size*))
	(finY (- (+ departY *sqrt-size*) 1)))
    (loop for x from departX to finX do
      (loop for y from departY to finY do
	;; différent de la coordonné de départ
	(unless (and (eq x coorX)
		     (eq y coorY))
	  (setf list (remove-sublist (get-possibility game x y) list)))))
    (unless (endp list)
      list))
  )


(defun one-possibility-in-line (game x y)
  "Retourne x y value, s'il y a une possibilité unique dans la line/column.
Par exemple si une value n'apparait que dans les possibilités d'un seul square de la line/column"
  (let ((list (remove-duplicates (append (verifier-line game x y 'line)
					 (verifier-line game x y 'column)))))
    (when (= (length list) 1) 
      (return-from one-possibility-in-line (values x y (car list)))))
  )


(defun verifier-line ( game coorX coorY &optional (sens 'line))
  "Vérifie l'unicité d'un nombre dans une line/column. 
L'optional sens permet de choisir entre line et colonne (line / column)"
  (assert (or (eq sens 'line) (eq sens 'column)))
  (let ((list (get-possibility game coorX coorY))
	(indiceStatic (if (eq sens 'line)
			  coorX
			  coorY)))    
    (loop for indiceMovible from 0 to (1- *size*) do    
       ;; pour parcourir les colonnes c'est le x qui est statique 
       ;; pour parcourir les lignes c'est le y qui est statique
	 (let ((coor (line-column sens indiceMovible indiceStatic)))
	   (unless (and (eq (car coor) coorX)
			(eq (cadr coor)  coorY))
	     (setf list (remove-sublist (get-possibility game (car coor) (cadr coor)) list)))))
    (unless (endp list)
      list))
  )


(defun depth-study-possibility (game x y)
  "Renvoie un x y value, permettant de résoudre la grille (étude en profondeur)"
  (loop for v in (get-possibility game x y) do
       (if (depht-hypothesis game x y v)
	   (return-from depth-study-possibility (values x y v))))
  )


(defun depht-hypothesis(game x y value)
  "Vérifie si à la coordonnée (x,y), la value permet de résoudre la grille"
  (let ((oldSquares (copy-squares (game-squares game)))
	(bool NIL))
    (change-digit game x y value)
    (update-possibility-all-square (game-squares game))
    ;; Vérifie qu'avec x y value on resoud le sudoku
    (when (eq (play-verification-game-over game) 'win)
      (setf bool T))
    
    (setf (game-squares game) oldSquares)
    bool)
  )


(defun play-verification-game-over (game)
  "Joue jusqu'à perdre ou gagner pour vérifier si x y value de l'étude en profondeur va résoudre la grille"
  (loop while T do
       (multiple-value-bind (x y value) (substrategy game '(one-possibility-square
							    one-possibility-in-line
							    one-possibility-in-subsquare))
	 (unless x
	   (return-from play-verification-game-over (game-over game)))
	 (change-digit game x y value)
	 (update-after-change-digit game x y)))
  )


(defun play-strategy (game)
  "Permet de lancer l'IA"
  (loop while (and (not (eq (strategy game) NIL))
		   (not (game-over game))) do
       (main-standalone)
       )
  (game-over game)
  )

