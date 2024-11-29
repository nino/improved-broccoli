(defpackage :loop-evolution
  (:use :cl))

(in-package :loop-evolution)

(defparameter *width* 100)
(defparameter *height* 30)
(defparameter *jungle* '(45 10 10 10))
(defparameter *plant-energy* 80)
(defparameter *reproduction-energy* 200
  "Energy required for an animal to reproduce.")

(defparameter *plants* (make-hash-table :test #'equal)
  "Map x/y coordinates to their plants.")

(defun random-plant (left top width height)
  "Place a plant at a random spot inside the rectangle given by LEFT, TOP,
  WIDTH, HEIGHT."
  (let ((pos (cons (+ left (random width)) (+ top (random height)))))
    (setf (gethash pos *plants*) t)))

(defun add-plants ()
  (apply #'random-plant *jungle*)
  (random-plant 0 0 *width* *height*))

(defstruct animal x y energy dir genes sym)


(defun make-circular (lst)
  "Helper function to make a list circular"
  (setf (cdr (last lst)) lst))

(defparameter *animal-symbols* (make-circular (map 'list #'identity "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZαβγδεζηθικλμνξοπρστυφχψωΑΒΓΔΕΖΗΘΙΚΛΜΝΞΟΠΡΣΤΥΦΧΨΩℵℶℷℸאבגדהוזחטיכלמנסעפצקרשתךםןףץ₠₡₢₣₤₥₦₧₨₩₪₫€₭₮₯₰₱₲₳₴₵₶₷₸₹₺₻₼₽₾₿÷×±≠≈≤≥∞∫∑∏√∂∆∇∈∉∋∌∩∪⊂⊃⊄⊅∀∃∄¬∨∧⊕⊗∥⊥∠∡∢⌈⌉⌊⌋⟨⟩♠♡♢♣♤♥♦♧☀☁☂☃★☆☎☏⚐⚑⚒⚔⚕⚖⚗⚘⚙⚚⚛⚜⚝⚞⚟")))

(defun next-sym ()
  (let ((sym (first *animal-symbols*)))
    (setf *animal-symbols* (rest *animal-symbols*))
    sym))

(defparameter *animals*
  (list (make-animal :x      (truncate (/ *width*  2))
                     :y      (truncate (/ *height* 2))
                     :energy 1000
                     :dir    0
                     :genes  (loop repeat 8
                                   collecting (1+ (random 10)))
                     :sym    (next-sym))))

(defun move (animal)
  (let ((dir (animal-dir animal))
        (x   (animal-x animal))
        (y   (animal-y animal)))
    ; Move x
    (setf (animal-x animal) (mod (+ x
                                    (cond ((and (>= dir 2) (<= dir 4)) 1)
                                          ((or (= dir 0) (>= dir 6)) -1)
                                          (t 0))
                                    *width*)
                                 *width*))
    ; Move y
    (setf (animal-y animal) (mod (+ y
                                    (cond ((and (>= dir 0) (<= dir 2)) -1)
                                          ((or (>= dir 4) (<= dir 6)) 1)
                                          (t 0))
                                    *height*)
                                 *height*))
    ; Hunger
    (decf (animal-energy animal) 1)))

(defun turn (animal)
  "Based on the weighting provided by the genes, turn right by a random amount."
  (let ((x (random (apply #'+ (animal-genes animal)))))
    (labels ((angle (genes x)
               (let ((xnu (- x (car genes))))
                 (if (< xnu 0)
                     0
                     (1+ (angle (cdr genes) xnu))))))
      (setf (animal-dir animal)
            (mod (+ (animal-dir animal) (angle (animal-genes animal) x))
                 8)))))

(defun animal-pos (animal)
  "Get the animal's coordinates as an x/y pair."
  (cons (animal-x animal) (animal-y animal)))

(defun eat (animal)
  "Eat any plant that might be at the animal's position."
  (let ((pos (animal-pos animal)))
    (when (gethash pos *plants*)
      (incf (animal-energy animal) *plant-energy*)
      (remhash pos *plants*))))

(defun reproduce (animal)
  (let ((energy (animal-energy animal)))
    (when (>= energy *reproduction-energy*)
      (setf (animal-energy animal) (truncate (/ energy 2)))
      (let ((animal-nu (copy-structure animal))
            (genes     (copy-list (animal-genes animal)))
            (mutation  (random 8)))
        (setf (nth mutation genes)
              ; Change the gene up or down by 1
              (max 1 (+ (nth mutation genes) (random 3) -1)))
        (setf (animal-genes animal-nu) genes)
        (setf (animal-sym animal-nu) (next-sym))
        (push animal-nu *animals*)))))


(defun update-world ()
  (setf *animals* (remove-if (lambda (animal)
                               (<= (animal-energy animal) 0))
                             *animals*))
  (mapc (lambda (animal)
          (turn animal)
          (move animal)
          (eat animal)
          (reproduce animal))
        *animals*)
  (add-plants))

(defun animal-at (x y)
  (find-if (lambda (animal)
             (and (= (animal-x animal) x)
                  (= (animal-y animal) y)))
           *animals*))

(defun clear-screen ()
  (format t "~c[2J~c[H" #\ESC #\ESC))

(defun draw-world ()
  (clear-screen)
  (dotimes (i (+ 2 *width*))
    (princ "-"))
  (loop for y below *height*
        do (progn (fresh-line)
                  (princ "|")
                  (loop for x below *width*
                        do (let ((animal (animal-at x y)))
                             (princ (cond (animal (animal-sym animal))
                                          ((gethash (cons x y) *plants*) #\*)
                                          (t #\space)))))
                  (princ "|")))
  (fresh-line)
  (dotimes (i (+ 2 *width*))
    (princ "-")))

(defun evolution ()
  (draw-world)
  (fresh-line)
  (let ((str (read-line)))
    (cond ((equal str "quit") ())
          (t (let ((x (parse-integer str :junk-allowed t)))
               (if x
                   (loop for i below x
                         do (update-world)
                         if (zerop (mod i 1000))
                         do (princ #\.))
                   (update-world))
               (evolution))))))

(evolution)




; ----



