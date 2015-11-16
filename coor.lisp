(in-package :sudoku)
(defvar *sqrt-size* 3 "side of the side of a zone")
(defvar *size* (* *sqrt-size* *sqrt-size*))
(defvar *nb-squares* (* *size* *size*))
(defvar *list-alpha* '(a b c d e f g h i j k l m n o p q r s t u v w x y))

(defgeneric x-coor (coor)
  (:documentation "x slot of coor"))

(defgeneric y-coor (coor)
  (:documentation "y slot of coor"))

(define-condition quit-interactive (condition)
  ())

(defclass coor ()
  ((x :initarg :x :reader x-coor :type integer)
   (y :initarg :y :reader y-coor :type integer))
  (:documentation "class for coordinates in the squares grid"))

(defmethod print-object ((coor coor) stream)
  (format stream "[~A,~A]" (x-coor coor) (y-coor coor)))

(defgeneric make-coor (x y)
  (:documentation "instance of a coor [x,y]"))

(defmethod make-coor ((x integer) (y integer))
  (make-instance 'coor :x x :y y))

(defvar *all-coors*
  (loop for i from 0 below *size*
     nconc (loop 
	      for j from 0 below *size*
	      collect (make-coor i j))))


(defvar *line-coors* (make-array *size*))
(loop for l from 0 below *size*
     do (setf (aref *line-coors* l)
	      (loop 
		 for c from 0 below *size*
		   collect (make-coor l c))))

(defvar *column-coors* (make-array *size*))
(loop for c from 0 below *size*
     do (setf (aref *column-coors* c)
	      (loop 
		 for l from 0 below *size*
		   collect (make-coor l c))))

(defun make-coor-from-pair (pair)
  (apply #'make-coor pair))

(defun correct-line (indice)
  (< -1 indice *size*))

(defvar *first-digit* 49)
(defvar *first-letter* 65)

(defun x-to-int-char (x)
  (code-char (+ x *first-digit*)))

(defun int-char-to-x (c)
  (- (char-code c) *first-digit*))

(defun alpha-char-to-y (c)
  (- (char-code c) *first-letter*))

(defun y-to-alpha-char (y)
  (code-char (+ y *first-letter*)))

(defun correct-column-char (c)
  (< -1 (alpha-char-to-y c)  *size*))

(defvar *column-string* "C")
(defvar *line-string* "L")

(defun prompt-square ()
  (format t "~A ~A? " *column-string* *line-string*))

(defun prompt-line ()
  (format t "~A? " *line-string*))

(defgeneric print-coor ( coor &optional stream))
(defmethod print-coor ((coor coor) &optional (stream *standard-output*))
  (write-char (y-to-alpha-char (y-coor coor)) stream)
  (write-char (x-to-int-char (x-coor coor)) stream))
   
(defun read-x ()
  (prompt-line)
  (let ((x (parse-integer (read-line) :junk-allowed t)))
    (if (and x (integerp x) (correct-line (decf x)))
	x
	(read-x))))

(defun human-read-coor ()
  (loop
     initially (prompt-square)
     with x = nil
     with y = nil
     for line = (read-line) then (read-line)
     if (string= line "")
       do (signal 'quit-interactive)
     else
       do (let ((c (with-input-from-string (stream line)
		     (read-char stream t t nil))))
	    (if (and c (setf c (char-upcase c)) (correct-column-char c))
		(progn
		  (setf y (alpha-char-to-y c))
		  (setf x (parse-integer (subseq line 1) :junk-allowed t))
		  (unless (and x (integerp x) (correct-line (decf x)))
		    (setf x (read-x))))
		(progn
		  (format t "Incorrect column letter~%"))))
     until (and x y)
     finally (return (make-coor x y))))

(defun machine-read-coor ()
  (let ((yc (read-char))
	(xc (read-char)))
    (make-coor (int-char-to-x xc) (alpha-char-to-y yc))))

(defun random-coor (nb-lines nb-columns)
  (make-coor (random nb-lines) (random nb-columns)))

(defgeneric coor= (coor1 coor2))
(defmethod coor= ((coor1 coor) (coor2 coor))
  (and (= (x-coor coor1) (x-coor coor2))
       (= (y-coor coor1) (y-coor coor2))))

(defgeneric coor+ (coor1 coor2))
(defmethod coor+ ((coor1 coor) (coor2 coor))
  (make-coor (+ (x-coor coor1) (x-coor coor2))
	     (+ (y-coor coor1) (y-coor coor2))))

(defun coor-member (coor coors)
  (member coor coors :test #'coor=))

;; zcoor index in array of zones
;; coor index in array size x size
;; rcoor relative index in a zone
;;
;;            | 0 1 2| 3 4 5| 6 7 8| absolute
;;            ----------------------
;;            |   0  |   1  |   2  | zone coor
;;            ----------------------
;;            | 0 1 2| 0 1 2| 0 1 2| relative in zone
;; ---------------------------------
;;  0 |   | 0 |      |      |      |
;;  1 | 0 | 1 |  Z0  |  Z1  |  Z2  |
;;  2 |   | 2 |      |      |      |
;; ---------------------------------
;;  3 |   | 0 |      |      |      |
;;  4 | 1 | 1 |  Z3  |  Z4  |  Z5  |
;;  5 |   | 2 |      |      |      |
;; ---------------------------------
;;  6 |   | 0 |      |      |      |
;;  7 | 2 | 1 |  Z6  |  Z7  |  Z8  |
;;  8 |   | 2 |      |      |      |
;; ---------------------------------
;; Example: the square of absolute coordinate [5,4] is in zone 4, with relative coordinate [2,1]
 
(defun absolute-to-relative (x)
  "relative index (in zone) to absolute index (in grid)"
  (floor (/ x *sqrt-size*)))

(defun relative-to-absolute (x)
  "absolute index (in grid) to relative index (in zone)"
  (* x *sqrt-size*))

(defgeneric zcoor-to-zone (zcoor)
  (:documentation
   "zone corresponding to the zone coordinate ZCOOR"))

(defmethod zcoor-to-zone ((zcoor coor))
  (+ (* (x-coor zcoor) *sqrt-size*) (y-coor zcoor)))

(defgeneric coor-to-zone (coor) 
  (:documentation "zone of coor"))

(defmethod coor-to-zone ((zcoor coor))
  (zcoor-to-zone
   (make-coor
    (absolute-to-relative (x-coor zcoor))
    (absolute-to-relative (y-coor zcoor)))))

(defun zone-to-zcoor (zone)
  "zone coor of ZONE"
  (multiple-value-call  #'make-coor (floor zone *sqrt-size*)))

(defun zone-to-coor (zone)
  "the absolute coor of the top-left square in ZONE"
  (let ((zcoor (zone-to-zcoor zone)))
    (make-coor
     (relative-to-absolute (x-coor zcoor))
     (relative-to-absolute (y-coor zcoor)))))

(defgeneric rcoor-to-coor (zone rcoor)
  (:documentation "from zone and relative coor in zone to absolute coor"))

(defmethod rcoor-to-coor ((zone integer) (rcoor coor))
  (coor+ rcoor (zone-to-coor zone)))

(defgeneric coor-to-rcoor (coor))
(defmethod coor-to-rcoor ((coor coor))
  (make-coor
   (absolute-to-relative (x-coor coor))
   (absolute-to-relative (y-coor coor))))

(defvar *zone-coors* (make-array *size*))
(loop
   for zone from 0 below *size* 
   do (setf (aref *zone-coors* zone)
	    (loop
	       for coor in *all-coors*
	       when (= (coor-to-zone coor) zone)
	       collect coor)))

(defvar *forbiden-coors* (make-array (list *size* *size*)))
(loop
   for coor in *all-coors*
   do (setf (aref *forbiden-coors* (x-coor coor) (y-coor coor))
	    (aref *zone-coors* (coor-to-zone coor))))
     
(defgeneric conflicting-coors (coor)
  (:documentation "list of coordinates in conflict with COOR"))

(defmethod conflicting-coors ((coor coor))
  (remove coor
	  (append (aref *line-coors* (x-coor coor))
		  (aref *column-coors* (y-coor coor))
		  (aref *zone-coors* (coor-to-zone coor)))
	  :test #'coor=))
