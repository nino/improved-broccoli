(ql:quickload 'usocket)

(defparameter my-socket (usocket:socket-listen #(127 0 0 1) 4123))
(defparameter my-stream (usocket:socket-stream (usocket:socket-accept my-socket)))
(read my-stream)
(close my-stream)
