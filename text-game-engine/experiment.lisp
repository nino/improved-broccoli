(in-package :cl-user)
(defpackage :experimenat
  (:use :cl :cl-arrows))
(in-package #:experimenat)
(ql:quickload "cxml")

(defparameter *doc* nil)

(setq *doc*
      (cxml:parse-file "/Users/Nino/Documents/Manually compiled ebooks/Paddington/Paddington 9, 1.sla"
                       (cxml-dom:make-dom-builder)))

(elt (dom:child-nodes (elt (dom:child-nodes *doc*) 0)) 1)
(-> *doc* (dom:child-nodes) (elt 0) (dom:child-nodes))
