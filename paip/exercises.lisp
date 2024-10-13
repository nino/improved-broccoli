;; Exercise 1.1 [m]  Define a version of `last-name` that handles "Rex Morgan
;; MD", "Morton Downey, Jr.", and whatever other cases you can think of.

(defun original-last-name (name)
  "Select the last name from a name represented as a list."
  (first (last name)))

(defvar *name-appendages* '(md jr sr phd))

(defun last-name (name)
  "Select the last name from a name represented as a list.
   This function ignores honorifics and other appendages
   that might go at the end of the sentence."
  (let ((rev (reverse name)))
    (labels ((drop-appendages (items)
               (cond
                 ((not items) items)
                 ((member (first items) *name-appendages*) (drop-appendages (rest items)))
                 (t items))))
      (first (drop-appendages rev)))))

;; Exercise 1.2 [m]

(defun expo (basis exponent)
  "Calculate the EXPONENT'th power of BASIS."
  (cond
    ((= exponent 0) 1)
    ((< exponent 0) (* (/ 1 basis) (expo basis (1+ exponent))))
    (t (* basis (expo basis (- exponent 1))))))

;; Exercise 1.5 [m]

(defun dot-product (a b)
  (apply #'+ (mapcar #'* a b)))

