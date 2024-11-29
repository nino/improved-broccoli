(defparameter *player-health* nil)
(defparameter *player-agility* nil)
(defparameter *player-strength* nil)

(defparameter *monsters* nil)
(defparameter *monster-builders* nil)
(defparameter *monster-num* 12)

(defun orc-battle ()
  (init-monsters)
  (init-player)
  (game-loop)
  (when (player-dead?)
    (princ "You have been killed. Game over."))
  (when (monsters-dead?)
    (princ "Congratulations! You have vanquished all your foes.")))

(defmacro λ (args body)
  (if (and args (atom args)) (setf args (list args)))
  (if (and (consp body) (consp (car body))) (push 'funcall body))
  (if (null args)
    body
    `(lambda (&rest args1)
       (let ((,(first args) (first args1)))
         (declare (ignorable ,(first args)))
         (flet ((,(first args) (&rest args2)
                (apply ,(first args) args2)))
           (if (rest args1)
             (apply (λ ,(rest args) ,body) (rest args1))
             (λ ,(rest args) ,body)))))))

(defun game-loop ()
  (unless (or (player-dead?) (monsters-dead?))
    (show-player)
    (dotimes (k (1+ (truncate (/ (max 0 *player-agility*) 15))))
      (unless (monsters-dead?)
        (show-monsters)
        (player-attack)))
    (fresh-line)
    (map 'list (λ (m)
                 (or (monster-dead? m) (monster-attack m)))
         *monsters*)
    (game-loop)))

(defun init-player ()
  (setf *player-health* 30)
  (setf *player-agility* 30)
  (setf *player-strength* 30))

(defun player-dead? ()
  (<= *player-health* 0))

(defun show-player ()
  (fresh-line)
  (princ (concatenate 'string
                      "You are a valiant knight with a health of "
                      (write-to-string *player-health*)
                      ", an agility of "
                      (write-to-string *player-agility*)
                      ", and a strength of "
                      (write-to-string *player-strength*))))

(defun player-attack ()
  (fresh-line)
  (princ "Attack style: [s]tab; [d]ouble swing; [r]oundhouse:")
  (case (read)
    (s (monster-hit (pick-monster)
                    (+ 2 (randval (ash *player-strength* -1)))))
    (d (let ((x (randval (truncate (/ *player-strength* 6)))))
         (princ "Your double swing has a strength of ")
         (princ x)
         (fresh-line)
         (monster-hit (pick-monster) x)
         (unless (monsters-dead?)
           (monster-hit (pick-monster) x))))
    (otherwise (dotimes (x (1+ (randval (truncate (/ *player-strength* 3)))))
                 (unless (monsters-dead?)
                   (monster-hit (random-monster) 1))))))

(defun randval (n)
  (1+ (random (max 1 n))))

(defun random-monster ()
  (let ((monster (aref *monsters* (random (length *monsters*)))))
    (if (monster-dead? monster)
      (random-monster)
      monster)))

(defun pick-monster ()
  (fresh-line)
  (princ "Monster #:")
  (let ((choice (read)))
    (if (not (and (integerp choice) (>= choice 1) (<= choice *monster-num*)))
      (progn (princ "Invalid monster number.")
             (pick-monster))
      (let ((monster (aref *monsters* (1- choice))))
        (if (monster-dead? monster)
          (progn (princ "That monster is already dead.")
                 (pick-monster))
          monster)))))

(defun init-monsters ()
  (setf *monsters*
        (map 'vector
             (lambda (monster)
               (funcall (nth (random (length *monster-builders*))
                             *monster-builders*)))
             (make-array *monster-num*))))


(defun monster-dead? (monster)
  (<= (monster-health monster) 0))

(defun monsters-dead? ()
  (every #'monster-dead? *monsters*))

(defun show-monsters ()
  (fresh-line)
  (princ "Your foes:")
  (let ((idx 0))
    (map 'list
         (lambda (monster)
           (fresh-line)
           (format t "~4@a. " (incf idx))
           (if (monster-dead? monster)
             (princ "**dead**")
             (progn
               (princ (concatenate 'string
                                   "(Health="
                                   (write-to-string (monster-health monster))
                                   ") "))
               (monster-show monster))))
         *monsters*)))

(defstruct monster (health (randval 10)))
(make-monster)

(defmethod monster-hit (mon damage)
  (decf (monster-health mon) damage)
  (if (monster-dead? mon)
      (progn (princ (concatenate 'string "You killed the "))
             (princ (type-of mon))
             (princ "! "))
      (progn (princ "You hit the ")
             (princ (type-of mon))
             (princ ", knocking off ")
             (princ damage)
             (if (= 1 damage)
                 (princ " health point! ")
                 (princ " health points! ")))))

(defmethod monster-show (monster)
  (princ "A fierce ")
  (princ (type-of monster)))

(defmethod monster-attack (monster))

(defstruct (orc (:include monster)) (club-level (randval 8)))
(push #'make-orc *monster-builders*)

(defmethod monster-show ((mon orc))
  (princ "A wicked orc with a level ")
  (princ (orc-club-level mon))
  (princ " club."))

(defmethod monster-attack ((mon orc))
  (let ((power (randval (orc-club-level mon))))
    (princ "An orc swings their club at you and knocks off ")
    (princ power)
    (princ " of your health points. ")
    (decf *player-health* power)))

(defstruct (hydra (:include monster)))
(push #'make-hydra *monster-builders*)

(defmethod monster-show ((mon hydra))
  (princ "A malicious hydra with ")
  (princ (monster-health mon))
  (princ " heads."))

(defmethod monster-hit ((mon hydra) power)
  (decf (monster-health mon) power)
  (if (monster-dead? mon)
    (princ "The corpse of the fully decapitated hydra falls to the floor!")
    (progn (princ "You lop off ")
           (princ power)
           (princ " of the hydra's heads! "))))

(defmethod monster-attack ((mon hydra))
  (let ((power (randval (ash (monster-health mon) -1))))
    (princ "A hydra attacks you with ")
    (princ power)
    (princ " of its heats! It also grows back one more head! ")
    (incf (monster-health mon))
    (decf *player-health* power)))

(defstruct (slime-mold (:include monster)) (sliminess (randval 5)))
(push #'make-slime-mold *monster-builders*)

(defmethod monster-show ((mon slime-mold))
  (princ "A slime mold with a sliminess of ")
  (princ (slime-mold-sliminess mon))
  (princ "."))

(defmethod monster-attack ((mon slime-mold))
  (let ((x (randval (slime-mold-sliminess mon))))
    (princ "A slime mold wraps around your legs and decreases your agility by ")
    (princ x)
    (princ "! ")
    (decf *player-agility* x)
    (when (zerop (random 2))
      (princ "It also squirts in your face, taking away a health point! ")
      (decf *player-health*))))

(defstruct (brigand (:include monster)))
(push #'make-brigand *monster-builders*)

(defmethod monster-attack ((m brigand))
  (let ((x (max *player-health* *player-agility* *player-strength*)))
    (cond ((= x *player-health*)
           (princ "A brigand hits you with their slingshot, taking off 2 health points! ")
           (decf *player-health* 2))
          ((= x *player-agility*)
           (princ "A brigand catches your leg with their whip, taking off 2 agility points! ")
           (decf *player-agility* 2))
          ((= x *player-strength*)
           (princ "A brigand cuts your arm with their whip, taking off 2 strength points! ")
           (decf *player-strength* 2)))))



(orc-battle)
