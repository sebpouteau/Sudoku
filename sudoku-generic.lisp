
					;===========================================
					;==             Interface                 ==
					;==  contenant toutes les méthodes utile  ==
					;===========================================

(in-package :sudoku)
		
;; =================
;; =    Données    =
;; =================

(defvar *sqrt-size* 5 "side of the side of a zone")
(defvar *size* (* *sqrt-size* *sqrt-size*))
(defvar *nb-squares* (* *size* *size*))
(defvar *game* nil "the current instance of a game")
(defvar *digits* '(1 2 3 4 5 6 7 8 9))
;; ========================
;; ==     Coordonnée     ==
;; ========================

(defclass coor ()
  ((x :initarg :x :reader x-coor :type integer)
   (y :initarg :y :reader y-coor :type integer))
  (:documentation "class for coordinates in the squares grid"))

(defgeneric x-coor (coor)
  (:documentation "x slot of coor"))

(defgeneric y-coor (coor)
  (:documentation "y slot of coor"))

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
         possible-digits -> valeur (nombre) rentrée par l'utilisateur
         digit -> valeur (nombre) du carré
         protected -> vaut true si le nombre était au début => non-modifiable"))

(defgeneric make-square (coor &optional digit)
  (:documentation "creates a square containing coor and digit"))

(defgeneric coor-square (squares coor)
  (:documentation "the square at COOR in SQUARES"))

(defgeneric possible-digits (square)
  (:documentation 
   "list of possible digits remaining in SQUARE"))

(defgeneric digit (square)
  (:documentation 
   "assigned digit of square (0 if unassigned)"))

(defgeneric protected (square) 
  (:documentation 
   "T if square was originally filled in the grid"))

(defgeneric assigned-p (square)
  (:documentation "T if the digit slot a striclty positive and inferior to 10"))

;; ==================
;; ==     Grid     ==
;; ==================

(defclass squares ()
  ((squares-array :initarg :squares-array :reader squares-array :type array
		  :initform (make-squares-array *size*))
   (to-fill :initform *nb-squares* :accessor to-fill))
  (:documentation "class containing a two dimensional array of instances of the square class \
                   to-fill -> nombre de cases vides (= à remplir)"))

(defgeneric to-fill (squares)
  (:documentation "return number of free case"))
		  
(defgeneric make-squares-array (size)
  (:documentation "return array two dimensional (size,  size) "))

(defgeneric squares-array (size)
  (:documentation "reader squares-array "))

(defgeneric grid-to-square (squares)
  (:documentation "attribue the right coordinates to square-array"))


;; ==================
;; ==     Game     ==
;; ==================

(defclass game ()
  ((game-squares :accessor game-squares :initarg :game-squares)
   (initial-grid :reader initial-grid :initarg :initial-grid))
  (:documentation "class for game instances"))

(defgeneric game-squares (game)
  (:documentation "the squares of GAME"))

(defgeneric initial-grid (game)
  (:documentation "the initial grid of GAME"))

(defgeneric game-over (game)
  (:documentation "if the game is over (either won or lost"))

(defgeneric init-game (game)
  (:documentation "initializes GAME with its initial-grid"))

(defgeneric game-with-new-grid (&optional strategy)
  (:documentation "instance of game with a grid and STRATEGY"))

(defgeneric init-sudoku ()
  (:documentation "to initialize whatever you want to initialize"))

(defgeneric game-do (game square)
  (:documentation 
   "plays coor/digit of square in the coor-square in squares of GAME"))

