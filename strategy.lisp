;; =====================
;; ==     Données     ==
;; =====================

(defun create-list-possibility (size list)
  (if (= size 0)
      list
      (create-list-possibility (1- size) (cons size list))))

(defvar *sqrt-size* 3 "side of the side of a zone")
(defvar *size* (* *sqrt-size* *sqrt-size*))
(defvar *nb-squares* (* *size* *size*))
(defvar *game* nil "the current instance of a game")
(defvar *digits* (create-list-possibility *size* '()))



;; =========================
;; ==     Coordonnées     ==
;; =========================

(defclass coor ()
  ((x :initarg :x :reader x-coor :type integer)
   (y :initarg :y :reader y-coor :type integer))
  (:documentation "class for coordinates in the squares grid"))

(defgeneric make-coor (x y)
  (:documentation "instance of a coor [x,y]"))


;; ===================
;; ==     Carre     ==
;; ===================

(defclass square ()
  ((coor :initarg :coor :accessor coor) 
   (possible-digits :initform (copy-list *digits*)
		    :initarg :possible-digits :accessor possible-digits)
   (digit :initarg :digit :accessor digit :initform 0)
   (protected :initform nil :initarg :protected :accessor protected))
  (:documentation "one square of the squares
         coor -> coordonnée du carré dans la grille
         possible-digits -> valeurs (nombres) possibles du carré
         digit -> valeur (nombre) affiché du carré
         protected -> vaut true si le nombre était au début => non-modifiable"))

(defgeneric make-square (coor &optional digit)
  (:documentation "creates a square containing coor and digit"))

(defgeneric assigned-p (square)
  (:documentation "T if the digit slot a striclty positive and inferior to 10"))

(defgeneric copy-square(square)
  (:documentation "copie un square et retourne la copie"))


;; ====================
;; ==     Grille     ==
;; ====================

(defclass squares ()
  ((squares-array :initarg :squares-array :reader squares-array :type array
		  :initform (make-squares-array *size*))
   (to-fill :initform *nb-squares* :accessor to-fill))
  (:documentation "class containing a two dimensional array of instances of the square class \
                   to-fill -> nombre de cases vides (= à remplir)"))

(defgeneric make-squares()
  (:documentation "return array two dimensional (size,  size) "))


(defgeneric make-squares-array (size)
  (:documentation "return array two dimensional (size,  size) "))

(defgeneric grid-to-square (squares)
  (:documentation "attribue the right coordinates to square-array"))

(defgeneric change-digit (squares x y value)
  (:documentation "change la valeur digit du carré en (x,y)
                  et met a jour les possibilités de la ligne, colonne et du sous-carré")) 

(defgeneric copy-squares(squares)
  (:documentation "copie le squares passé en paramètre et retourne la copie"))


(defgeneric update-possibility (squares x y list)
  (:documentation "met à jour possible-digits du carré en (x,y) par rapport à list.
                   Il supprime les éléments de possible-digits présent dans list."))

(defgeneric remove-sublist (list1 list2)
  (:documentation "supprime les éléments de list2 présent dans list1 et renvoie list2"))

(defgeneric update-possibility-line (squares indiceStatic &optional sens comportement )
  (:documentation "cette fonction compote plusieurs fonctionnalités qui sont modulables grace aux deux paramètres optionnels:
                     sens -> 'line ou 'column permet de parcourir les lignes ou les colonnes
                     comportement -> 'list : renvoie les digits de la ligne ou colonne
                                  -> 'update-possibility : met à jour les possible-digits de tous les carrés d'une ligne ou colonne
                                  -> 'update-and-list : met a jour les possible-digits et renvoie la list des digits de la ligne ou colonne"))

(defgeneric update-possibility-subsquares (squares x y &optional comportement)
  (:documentation "cette fonction permet de gérer les sous-carrés selon le comportement choisi
                     comportement -> 'list : renvoie les digit du sous-carré
                                  -> 'update-possibility : met à jour les possible-digits de tout les carrés du sous-carré
                                  -> 'update-and-list : met à jour les possible-digits et renvoie la list des digits du sous-carré "))

(defgeneric update-possibility-all-square (squares)
  (:documentation "met à jour les possible-digits de tous les carrés de la grille passée en paramètre"))


;; ==================
;; ==     Game     ==
;; ==================

(defclass game ()
  ((game-squares :accessor game-squares :initarg :game-squares)
   (initial-grid :reader initial-grid :initarg :initial-grid))
  (:documentation "class for game instances"))

(defgeneric make-game (grid)
  (:documentation "création instance game avec initial-grid etant une grille 
                    (lien de la grille passé en parametre)"))

(defgeneric game-over (game)
  (:documentation "if the game qis over (either won or lost"))

(defgeneric init-game (game)
  (:documentation "initializes GAME with its initial-grid"))

(defgeneric game-with-new-grid (&optional strategy)
  (:documentation "instance of game with a grid and STRATEGY"))

(defgeneric game-do (game square)
  (:documentation 
   "plays coor/digit of square in the coor-square in squares of GAME"))

(defgeneric get-possibility (square x y)
  (:documentation "retourne la liste des possibilitées du carré (x,y)"))

(defgeneric print-grid (squares)
  (:documentation "affiche la grille du jeu"))


;;===========================================
;;==           Implémentation              ==
;;==          FONCTIONS SUDOKU             ==
;;===========================================



;; ========================
;; ==     Coordonnées    ==
;; ========================

(defmethod make-coor (x y)
  (make-instance 'coor :x x :y y))


;; ===================
;; ==     Carre     ==
;; ===================

(defmethod make-square (coor &optional digit)
  (make-instance 'square :coor coor :digit digit))


(defmethod assigned-p (square)
  (and (< 0 (digit square)) (<= (digit square) *size*)))


(defmethod copy-square(square)
  (let (( s (make-square
	     (coor square) (digit square)))) ;; s -> nouveau square
    (setf (possible-digits s) (possible-digits square)) ;; copie de possible-digits de square
    (setf (protected s) (protected square)) ;; copie de protected de square
    s))


;; =====================
;; ==     Squares     ==
;; =====================

(defmethod make-squares()
  (let (( squares (make-instance 'squares)))
    (grid-to-square squares)
    squares))


(defmethod make-squares-array (size)
  (make-array (list size size)))


(defmethod grid-to-square (squares)
  (loop for y from 0 to (1- *size*) do
       (loop for x from 0 to (1- *size*) do
	    (setf (aref (squares-array squares) x y)
		  (make-square (make-coor x y)  0)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmethod change-digit ((squares squares) x y value)
  (if (protected (aref (squares-array squares) x y))
    (print "Impossible de changer cette case, Valeur Initial")
    (unless ( = (digit (aref (squares-array squares) x y)) 0)
      (setf (digit (aref (squares-array squares) x y)) value)
      (update-possibility-all-square squares)))
      
  (unless (and (protected (aref (squares-array squares) x y))
	       (not (= (digit (aref (squares-array squares) x y)) 0)))
    (setf (digit (aref (squares-array squares) x y)) value)
    (setf (possible-digits (aref (squares-array squares) x y)) '())
    (update-possibility-line squares y 'line 'update-possibility)
    (update-possibility-line squares x 'column 'update-possibility)
    (update-possibility-subsquares squares x y 'update-possibility)
    (setf (to-fill squares) (1- (to-fill squares )))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  

(defmethod copy-squares(squares)
  (let ((s (make-squares)))
    ;; copie de tous les carrés de squares et stocke le résultat dans s
    (loop for y from 0 to (1- *size*) do
	 (loop for x from 0 to (1- *size*) do
	      (setf (aref (squares-array s) x y)
		    (copy-square (aref (squares-array squares) x y))
		    )
	      ))
    (setf (to-fill s) (to-fill squares)) ;; copie to-fill de square
    s))


(defmethod update-possibility (squares x y list)
  (if (eq (digit (aref (squares-array squares) x y)) 0) 
      (setf (possible-digits (aref (squares-array squares) x y))
	    (remove-sublist list (get-possibility squares x y)))
      (setf (possible-digits (aref (squares-array squares) x y))
	    NIL)))

(defmethod remove-sublist(list1 list2)
  (if (endp list1)
      list2
      (remove-sublist (cdr list1) (remove (car list1) list2))))


(defmethod update-possibility-line (squares indiceStatic &optional (sens 'line) (comportement 'update-possibility))
  (assert (or (eq comportement 'update-possibility) (eq comportement 'list-digits) (eq comportement 'update-and-list)))
  (assert (or (eq sens 'line) (eq sens 'column)))
  (labels ((line-column (x y)
	     (if (eq sens 'line)
		 (list y x)
		 (list x y))))
    (let ((list '())
	  (array (squares-array squares)))
      (loop for indiceMovible from 0 to (1- *size*) do    
	 ;; pour parcourir les colonnes c'est le x qui est statique 
	 ;; pour parcourir les lignes c'est le y qui est statique
	   (let* ((coor (line-column indiceMovible indiceStatic))
		  (value (digit(aref array (car coor) (cadr coor))))
		  )
	     (if (not (zerop value))
		 (setf list (cons value list)))))
      
      (unless (eq comportement 'list-digits)
	    (loop for indiceMovible from 0 to (1- *size*) do    
	      (let ((coor (line-column indiceMovible indiceStatic)))
	        (update-possibility squares (car coor) (cadr coor) list))))
      (unless (eq comportement 'update-possibility)
	     list))))


(defmethod update-possibility-subsquares (squares x y &optional (comportement 'update-possibility))
  (assert (or (eq comportement 'update-possibility) 
	      (eq comportement 'list-digits)
	      (eq comportement 'update-and-list)))
  (let ((list '())
	(array (squares-array squares))
	;; definition des débuts et fins du petit carré qui contient square (x,y)
	(departX (* (truncate (/ x *sqrt-size*)) *sqrt-size*))
	(finX (- (* (1+ (truncate (/ x *sqrt-size*))) *sqrt-size*) 1)) 
	(departY (* (truncate (/ y *sqrt-size*)) *sqrt-size*))
	(finY (- (* (1+ (truncate (/ y *sqrt-size*))) *sqrt-size*) 1)) )
    
    (loop for x from departX to finX do
	 (loop for y from departY to finY do
	      (let ((value (digit(aref array x y))))
		(if (not (zerop value))
		    (setf list (cons value list))))))
    (unless (eq comportement 'list-digits)
      (loop for x from departX to finX do
	   (loop for y from departY to finY do
		(update-possibility squares x y list))))
    (unless (eq comportement 'update-possibility)
      list)))


(defmethod update-possibility-all-square (squares)
  (loop for y from 0 to (1- *size*) do
    (loop for x from 0 to (1- *size*) do
      (setf (possible-digits (aref (squares-array squares) x y))
	    *digits*)))
  ;; met à jour les possible-digits de chaque carré, par rapport aux lignes et aux colonne
  (loop for x from 0 to (1- *size*) do
       (update-possibility-line squares x 'line 'update-possibility)
       (update-possibility-line squares x 'column 'update-possibility)
       )
  ;; met a jour les possible-digits des 9 sous-carrés
  (loop for y from 0 to (1- *sqrt-size*) do
       (loop for x from 0 to (1- *sqrt-size*) do
	    (update-possibility-subsquares squares 
					   (* x *sqrt-size*) 
					   (* y *sqrt-size*) 
					   'update-possibility))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun list-to-2d-array (list)
  (make-array (list (length list)
                    (length (first list)))
              :initial-contents list))

(defmethod make-game (grid)
  (make-instance 'game
		 :game-squares (make-squares)
		 :initial-grid (list-to-2d-array
				(read (open grid)))))


			   
(defmethod init-game (game)
  (setf (to-fill (game-squares game)) (* *size* *size*))
  (loop for x from 0 to (1- *size*) do
       (loop for y from 0 to (1- *size*) do
	      (setf (digit (aref (squares-array (game-squares game))
			  x y))
		    (aref (initial-grid game) x y))
	    (unless (zerop (aref (initial-grid game) x y))
	      (setf (to-fill (game-squares game)) (1- (to-fill (game-squares game))))
	      (setf (possible-digits (aref (squares-array (game-squares game)) x y)) '() )
  	      (setf (protected (aref (squares-array (game-squares game)) x y)) T ))))
  (update-possibility-all-square (game-squares game)))

(defmethod change-digit ((squares game) x y value)
  (change-digit (game-squares squares) x y value))

(defmethod get-possibility ((square game) x y)
  (possible-digits (aref (squares-array (game-squares square)) x y)))
(defmethod get-possibility ((square squares) x y)
  (possible-digits (aref (squares-array square) x y)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



(defmethod game-over(game)
  (block fin
      (loop for x from 0 to (1- *size*) do
	(loop for y from 0 to (1- *size*) do
	  (let ((square (aref (squares-array (game-squares game))
			       x y)))
	    (cond ((and (= (digit square) 0)
			(eq (possible-digits square) NIL))
		  
		   (return-from fin 'lose))
		   
		  
		  ((= (to-fill (game-squares game)) 0)
		  
		   (return-from fin 'win))
	    ))))))
  

;;===========================================
;;==           Implementation              ==
;;==             Fonction IA               ==
;;===========================================


(defmethod strategy (game)
  (update-possibility-all-square (game-squares game))
  (block fin
    (loop for fun in '(one-possibility-square one-possibility-in-subsquare one-possibility-in-line depth-study-possibility) do
	 (multiple-value-bind (x y value)  (funcall fun game)
	   (when x
	     (return-from fin (values x y value)))))))


(defun one-possibility-square (game)
  (block fin
    (loop for y from 0 to (1- *size*) do
	 (loop for x from 0 to (1- *size*) do
	      (when (and (not (protected ( aref (squares-array (game-squares game)) x y)))
			 (= 1 (length (get-possibility game x y)))
			 (eq (digit (aref (squares-array (game-squares game)) x y)) 0))
		(return-from fin (values x y (car (get-possibility game x y)))))))
    ))

(defun one-possibility-in-subsquare (game)
  (block fin
    (loop for y from 0 to (1- *size*) do
	 (loop for x from 0 to (1- *size*) do
	      (let ((list (verifier-subsquare game x y )))
		(if (and (= (length list) 1)
			 (not (protected ( aref (squares-array (game-squares game)) x y)))
			 (eq (digit (aref (squares-array (game-squares game)) x y)) 0))
		    (return-from fin (values x y (car list)))))))))

(defun one-possibility-in-line (game)
  (block fin
    (loop for y from 0 to (1- *size*) do
      (loop for x from 0 to (1- *size*) do
    	(let ((listL (verifier-line game x y 'line))
    	      (listC (verifier-line game x y 'column)))3
    	  (when (and (not (protected ( aref (squares-array (game-squares game)) x y)))
    		     (eq (digit (aref (squares-array (game-squares game)) x y)) 0))
    	    (when (= (length listL) 1)
    	      (return-from fin (values x y (car listL))))
    	    (when (= (length listC) 1)
    	      (return-from fin (values x y (car listC))))	    
    	    ))))))

(defun depth-study-possibility (game)
  (block fin
    (loop for y from 0 to (1- *size*) do
         (loop for x from 0 to (1- *size*) do
	      (when (and (not (protected ( aref (squares-array (game-squares game)) x y)))
			 ;(not (eq (get-possibility game x y) NIL))
			 (eq (digit (aref (squares-array (game-squares game)) x y)) 0))
		(loop for v in (get-possibility game x y) do
		     (if (depht-hypothesis game x y v)
			 (return-from fin (values x y v)))))))))
			 
(defun verifier-subsquare(game coorX coorY)
  (let ((list (get-possibility game coorX coorY))
	;; definition des débuts et fins du petit carré qui contient square (x,y)
	(departX (* (truncate (/ coorX *sqrt-size*)) *sqrt-size*))
	(finX (- (* (1+ (truncate (/ coorX *sqrt-size*))) *sqrt-size*) 1)) 
	(departY (* (truncate (/ coorY *sqrt-size*)) *sqrt-size*))
	(finY (- (* (1+ (truncate (/ coorY *sqrt-size*))) *sqrt-size*) 1)) )
    (loop for x from departX to finX do
      (loop for y from departY to finY do
	;; si non protéger et différent de a coordonné de départ
	(unless (and (eq x coorX)
		     (eq y coorY))
	  (setf list (remove-sublist (get-possibility game x y) list)))
	    ))
    (unless (endp list)
      list)
    ))


(defun verifier-line ( game coorX coorY &optional (sens 'line))
  (assert (or (eq sens 'line) (eq sens 'column)))
  (labels ((line-column (x y)
	     (if (eq sens 'line)
		 (list y x)
		 (list x y))))
    (let ((list (get-possibility game coorX coorY))
	  (indiceStatic (if (eq sens 'line)
			    coorX
			    coorY)))
      
	  (loop for indiceMovible from 0 to (1- *size*) do    
	 ;; pour parcourir les colonnes c'est le x qui est statique 
	    ;; pour parcourir les lignes c'est le y qui est statique
	    (let* ((coor (line-column indiceMovible indiceStatic)))
		   (unless (and (eq (car coor) coorX)
				    (eq (cadr coor)  coorY))
			       ;(protected ( aref (squares-array (game-squares game)) (car coor ) (cadr coor))))
		     (setf list (remove-sublist (get-possibility game (car coor) (cadr coor)) list)))
		   ))
	  (unless (endp list)
	    list))))

      
(defun jeu (game)
  (block fin
    (loop while (and (not (eq (strategy game) NIL))
		     (not (game-over game))) do
      (multiple-value-bind (x y value) (strategy game)
	(change-digit game x y value))
      (print-grid game)
      )))

(defun play-verification-game-over (game)
 
    (loop while (not (eq (strategy game) NIL)) do
      (multiple-value-bind (x y value) (strategy game)
	(change-digit game x y value))       
	  )
  (game-over game))

(defun depht-hypothesis(game x y value &optional (strong NIL))
  (let ((oldSquares (copy-squares (game-squares game)))
	(bool NIL))
    (change-digit game x y value)
    (update-possibility-all-square (game-squares game))
    
    (when (eq (play-verification-game-over game) 'win)
      (setf bool T))
    ;; si un seul niveau de parcour en pronfondeur ne suiffit pas
    (when (and strong (eq (play-verification-game-over game) NIL))
      (multiple-value-bind (x)  (depth-study-possibility game)
	(when x
	  (setf bool T))))
      
      (setf (game-squares game) oldSquares)
    bool
    )
  )

(defun init-standalone (grid)
  (setf *game* (make-instance 'game
		 :game-squares (make-squares)
		 :initial-grid grid))
  (init-game *game*)
  )

(defun main-standalone ()
  (strategy *game*))
