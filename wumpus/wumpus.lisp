(defpackage #:wumpus
  (:use :cl :cl-arrows))

(in-package #:wumpus)

(defstruct edge
  from ; ID of the source place
  to ; ID of the destination place
  cops?)

(defstruct place
  id
  clues)

(defparameter *congestion-city-nodes* nil
  "Places in Congestion City")

(defparameter *congestion-city-edges* nil
  "Roads")

(defparameter *visited-nodes* nil)

(defparameter *node-num* 30)
(defparameter *edge-num* 45)
(defparameter *worm-num* 3)
(defparameter *cop-odds* 15)

(defun random-node ()
  "Generate a random node ID."
  (1+ (random *node-num*)))

(defun edge-pair (a b)
  "Generate two edges: a -> b and b -> a."
  (unless (eql a b)
    (list (make-edge :from a :to b) (make-edge :from b :to a))))

(defun make-edge-list ()
  "Generate random edges."
  (apply #'append (loop repeat *edge-num*
                        collect (edge-pair (random-node) (random-node)))))

(make-edge-list)
