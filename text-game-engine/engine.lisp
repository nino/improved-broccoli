; Chapter 5 of Land of Lisp

(ql:quickload "fiveam")
(load "hashtbl.lisp")

(defparameter *nodes*
  (hashtbl-from-alist '((living-room (you are in the living-room.
                                          a wizard is snoring loudly on the couch.))
                        (garden (you are in a beautiful garden.
                                     there is a well in front of you.))
                        (attic (you are in the attic.
                                    there is a giant welding torch in the corner.)))))

;; Edge: (destination direction means)

(defparameter *edges*
  (hashtbl-from-alist '((living-room ((garden west door)
                                      (attic upstairs ladder)))
                        (garden ((living-room east door)))
                        (attic ((living-room downstairs ladder))))))

(defparameter *objects* '(whiskey bucket frog chain))

(defparameter *object-locations* (hashtbl-from-alist '((whiskey living-room)
                                                       (bucket living-room)
                                                       (chain garden)
                                                       (frog garden))))

(defparameter *location* 'living-room)

(defun objects-at (loc objects object-locations)
  (labels ((at-loc-p (obj)
                     (eq (gethash obj object-locations) loc)))
    (remove-if-not #'at-loc-p objects)))

(defun describe-location (location nodes)
  (gethash location nodes))

(defun describe-path (edge)
  `(there is a ,(caddr edge) going ,(cadr edge) from here.))

(defun describe-paths (location edges)
  (apply #'append (mapcar #'describe-path (gethash location edges))))

(defun describe-objects (location objects object-locations)
  ;; You can't define a function called `DESCRIBE-OBJECT`, omg
  (labels ((describe-obj (obj)
                            `(you see a ,obj on the floor.)))
    (apply #'append (mapcar #'describe-obj (objects-at location
                                                       objects
                                                       object-locations)))))

(defun describe-movement (direction)
  `(you walk in the ,direction direction.))

(defun look ()
  (append (describe-location *location* *nodes*)
          (describe-paths *location* *edges*)
          (describe-objects *location* *objects* *object-locations*)))

(defun walk (direction)
  (let ((next-loc (find direction
                        (gethash *location* *edges*)
                        :key #'cadr)))
    (if next-loc
      (progn (setf *location* (car next-loc))
             (append (describe-movement direction)
                     (look)))
      '(you cannot go that way.))))

; (defun move-object)

(defun pickup (object)
  (cond ((member object
                 (objects-at *location* *objects* *object-locations*))
         (setf (gethash object *object-locations*) 'body)
         `(the ,object is now in your bag.))
        (t '(you cannot get that.))))

(defun inventory ()
  (list 'inventory (objects-at 'body *objects* *object-locations*)))

(look)
