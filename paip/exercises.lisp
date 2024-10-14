(ql:quickload "alexandria")
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
  "Compute the mathematical dot-product of two vectors, given as lists."
  (apply #'+ (mapcar #'* a b)))

(defun sentence () (append (noun-phrase) (verb-phrase)))
(defun noun-phrase () (append (Article) (adj*) (Noun) (pp*)))
(defun verb-phrase () (append (Verb) (noun-phrase)))
(defun Article () (one-of '(the a)))
(defun Noun () (one-of '(man ball woman table)))
(defun Verb () (one-of '(hit took saw liked)))
(defun Adj* ()
  (if (= (random 2) 0)
      nil
      (append (adj) (adj*))))

(defun pp* ()
  (if (random-elt '(t nil))
      (append (pp) (pp*))
      nil))

(defun pp () (append (prep) (noun-phrase)))
(defun adj () (one-of '(big little green blue adiabatic)))
(defun prep () (one-of '(to in by with on)))

(defun one-of (set) (list (random-elt set)))

(defun random-elt (choices) (elt choices (random (length choices))))

(sentence)

(defparameter *simple-grammar*
  '((sentence ⇒ (noun-phrase verb-phrase))
    (noun-phrase ⇒ (article noun))
    (verb-phrase ⇒ (verb noun-phrase))
    (article ⇒ the a)
    (noun ⇒ man woman ball table chair leaf tree)
    (verb ⇒ hit took saw liked ate threw drove))
  "A grammar for a trivial subset of English.")

(defvar *grammar* *simple-grammar*
  "The grammar used by `generate`. Initially, this is *simple-grammar*,
  but we can switch to other grammars.")

(assoc 'sentence *grammar*)

(defun rule-lhs (rule)
  "The left-hand side of a rule."
  (first rule))

(defun rule-rhs (rule)
  "The right-hand side of a rule."
  (cddr rule))

(defun rewrites (category)
  "Return a list of possible rewrites for this category."
  (rule-rhs (assoc category *grammar*)))

(defun generate (phrase)
  (cond ((listp phrase)
         (alexandria:mappend #'generate phrase))
        ((let ((choices (rewrites phrase)))
           (if choices
               (generate (random-elt choices)))))
        (t (list phrase))))

(generate 'sentence)
