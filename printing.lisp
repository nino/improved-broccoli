(ql:quickload 'arrow-macros)
(defpackage :printing
  (:use cl arrow-macros))

(in-package :printing)

(format nil "Add onion rings for only ~5@$ pounds more!" 1.5)

(loop for i from 1 upto 100
      do (format t "~14@a. Hello.~%" (random (* 100.00 i))))
