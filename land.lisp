(defparameter *small* 1)
(defparameter *big* 100)

(defun guess ()
  (floor (/ (+ *small* *big*) 2)))

(defun bigger ()
  (setf *small* (1+ (guess)))
  (guess))

(defun smaller ()
  (setf *big* (1- (guess)))
  (guess))


