; Chapter 5 of Land of Lisp

(ql:quickload "fiveam")
(ql:quickload "str")
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

(defun pickup (object)
  (cond ((member object
                 (objects-at *location* *objects* *object-locations*))
         (setf (gethash object *object-locations*) 'body)
         `(the ,object is now in your bag.))
        (t '(you cannot get that.))))

(defun inventory ()
  (list 'inventory (objects-at 'body *objects* *object-locations*)))

(look)

(defun game-read ()
  "Read a user command and turn it into an s-expression.
  The command is equivalent to an s-expression,
  expect you can leave out the leading and trailing parens,
  and you don't have to quote symbols."
  (let ((cmd (read-from-string
               (concatenate 'string "(" (read-line) ")"))))
    (flet ((quote-it (x) (list 'quote x)))
      (cons (first cmd) (mapcar #'quote-it (rest cmd))))))

(game-read)

(defparameter *allowed-commands* '(look walk pick-up inventory)
  "List of supported player actions.")

(defun game-eval (sexp)
  "Evaluate a game command, given as SEXP."
  (if (member (first sexp) *allowed-commands*)
      (eval sexp)
      '(I do not know that command.)))

(defun sym-to-str (sym should-be-capitalised)
  "Turn a symbol into a string.
  If SHOULD-BE-CAPITALISED, the first letter will be upper-case.
  Otherwise, the string will be all lower-case."
  (if should-be-capitalised
      (str:capitalize sym)
      (str:downcase sym)))

(defun ends-with-punctuation-p (text)
  "Return T if TEXT ends with a punctuation character."
  (str:s-member '("." "?" "!") (str:s-last text)))

(defun format-text (symbols)
  "Render a list of symbols as text.
  The first letter of every sentence will be capitalised."
  (let ((uppercase-next t))
    (flet ((process-one (sym)
             (let ((s (sym-to-str sym uppercase-next)))
               (setq uppercase-next (ends-with-punctuation-p s))
               s)))
      (str:join " " (mapcar #'process-one symbols)))))
