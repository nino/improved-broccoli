(ql:quickload :drakma)
(ql:quickload :yason)

(defpackage :hue
  (:use :cl :cl-arrows))

(in-package :hue)

(defun get-lights (hue-bridge-ip api-key &optional (state t))
  "Retrieve lights from the Philips Hue bridge.
   STATE defaults to T (on), use NIL for off lights."
  (let ((base-url (format nil "http://~A/api/~A/lights" hue-bridge-ip api-key)))
    (let ((response (drakma:http-request base-url :method :get)))
      (let ((lights (yason:parse response)))
        (loop for light-id being the hash-key of lights
              using (hash-value details)
              ; when (equalp (cdr (assoc :on (cdr (assoc :state details)))) state)
              when (equalp (->> details (assoc :state) cdr (assoc :on) cdr) state)
              collect (list light-id (cdr (assoc :name details))))))))

(defparameter *things* '((hello cool) (nah not-cool)))

(assoc 'hello *things*)
(defun assoc-val (key my-alist)
  (second (assoc key my-alist)))

(->> *things* (assoc-val 'hello))

(defun main ()
  "Example usage of get-lights."
  (let ((hue-bridge-ip "YOUR_HUE_BRIDGE_IP_ADDRESS")
        (api-key "YOUR_HUE_API_KEY"))
    (format t "On lights: ~A~%" (get-lights hue-bridge-ip api-key))
    (format t "Off lights: ~A~%" (get-lights hue-bridge-ip api-key nil))))

(main)

(defparameter *hashie* (make-hash-table))

(setf (gethash :one *hashie*) 1)
(setf (gethash :two *hashie*) 2)

(print *hashie*)

(loop for k being the hash-key of *hashie* collect (list k (gethash k *hashie*)))
