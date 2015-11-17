;;===========================================
;;==           Implementation              ==
;;==             Fonction IA               ==     
;;==            Intelligente               ==
;;===========================================


;; =============================
;; ==     Fonction Static     ==
;; =============================

(in-package :sudoku)

(defun strategy (game)
    ;; on essaye toute les fonctions pour trouver un coup à jouer
  (substrategy game '(one-possibility-square
		      one-possibility-in-line
		      one-possibility-in-subsquare
		      depth-study-possibility)))

(defun substrategy (game list)
  "lance la strategie avec les focntion de la list"
  (loop for fun in list do
    (multiple-value-bind (x y value)  (parcour-grid-with-function game fun)
      (when x
	(return-from substrategy (values x y value))))))

(defun verification-square-valid (game x y)
  "return si on peut jouer dans le square x y"
  (and (not (protected ( aref (squares-array (game-squares game)) x y)))
       (eq (digit (aref (squares-array (game-squares game)) x y)) 0)
       ))

(defun parcour-grid-with-function (game fun)
  " permet de parcourir la grille et d'appliquer une focntion a chaque square valide"
    (loop for y from 0 to (1- *size*) do
      (loop for x from 0 to (1- *size*) do
	(when (verification-square-valid game x y)
	  (multiple-value-bind (x y value) (funcall fun game x y)
	    (when x
	      (return-from parcour-grid-with-function (values x y value))))))))


(defun one-possibility-square (game x y)
  "retourne x y value, d'un square n'ayant qu'une possibilité"
    (when (= 1 (length (get-possibility game x y)))
      (return-from one-possibility-square (values x y (car (get-possibility game x y))))))


(defun one-possibility-in-subsquare (game x y)
  "retourn x y value, si il y a une possibilité unique dans un sous square 
par exemple si 1 n'apparais que dans les possibilité d'un seul square du sous squares"
    (let ((list (verifier-subsquare game x y )))
      (when (= (length list) 1)
	(return-from one-possibility-in-subsquare (values x y (car list))))))


(defun verifier-subsquare(game coorX coorY)
  "verifie unicité d'un nombre dans un sous squares"
  (let* ((list (get-possibility game coorX coorY))
	;; definition des débuts et fins du petit carré qui contient square (x,y)
	(departX (* (truncate (/ coorX *sqrt-size*)) *sqrt-size*))
	(finX (- (+ departX *sqrt-size*) 1)) 
	(departY (* (truncate (/ coorY *sqrt-size*)) *sqrt-size*))
	(finY (- (+ departY *sqrt-size*) 1)) )
    (loop for x from departX to finX do
      (loop for y from departY to finY do
	;; différent de la coordonné de départ
	(unless (and (eq x coorX)
		     (eq y coorY))
	  (setf list (remove-sublist (get-possibility game x y) list)))
	    ))
    (unless (endp list)
      list)
    ))


(defun one-possibility-in-line (game x y)
  "retourn x y value, si il y a une possibilité unique dans la line/column 
par exemple si 1 n'apparais que dans les possibilité d'un seul square de la line/column"
  (let ((list (remove-duplicates (append (verifier-line game x y 'line)
					 (verifier-line game x y 'column)))))
	     (when (= (length list) 1) 
	       (return-from one-possibility-in-line (values x y (car list))))))
  

(defun verifier-line ( game coorX coorY &optional (sens 'line))
  "verifie l'unicité d'un nombre dans une line/column. 
sens permet de choisir entre line et colonne (line / column)"
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
	  (setf list (remove-sublist (get-possibility game (car coor) (cadr coor)) list)))
	))
    (unless (endp list)
      list)))


(defun depth-study-possibility (game x y)
  "renvoie un x y value, permettant de resoudre la grille (étude en profondeur)"
    (loop for v in (get-possibility game x y) do
      (if (depht-hypothesis game x y v)
	  (return-from depth-study-possibility (values x y v)))))


(defun depht-hypothesis(game x y value)
  "verifie si x y value resout la grille"
  (print "j' fait une hypothèse")
  (let ((oldSquares (copy-squares (game-squares game)))
	(bool NIL))
    
      (change-digit game x y value)
      (update-possibility-all-square (game-squares game))
      ;; verifie que avec x y value on resout le sudoku
      (when (eq (play-verification-game-over game) 'win)
	(setf bool T))
      
      (setf (game-squares game) oldSquares)
      bool
      )
  )

(defun play-verification-game-over (game)
  "joue jusqu'a perdre ou gagner pour verifier si x y value de l'étude en profondeur va resoudre la grille"
    (loop while T do
      (multiple-value-bind (x y value) (substrategy game '(one-possibility-square
							   one-possibility-in-line
							   one-possibility-in-subsquare))
	(unless x
	  (return-from play-verification-game-over (game-over game)))
	(change-digit game x y value)
	(update-after-change-digit game x y)
	))
 )



(defun play-strategy (game)
  "permet de lancer l'IA"
  (loop while (and (not (eq (strategy game) NIL))
		   (not (game-over game))) do
	  (main-standalone)
	)
  (game-over game)
  )

 
(defun update-after-change-digit(game x y)
  "update les possibilité de la line,colonne, sous-carré en [x,y]"
  (update-possibility-subsquares (game-squares game) x y)
  (update-possibility-line (game-squares game) x 'line)
  (update-possibility-line (game-squares game) y 'column))
