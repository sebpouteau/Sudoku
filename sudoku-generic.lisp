
;;============================================
;;==             Interface                  ==
;;==  contenant toutes les méthodes utiles  ==
;;============================================

(in-package :sudoku)
		
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

(defgeneric game-over (game &key print)
  (:documentation "if the game is over (either won or lost"))

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

