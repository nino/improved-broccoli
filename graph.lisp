(defun dot-name (exp)
  "Convert any expression into a valid identifier for DOT."
  (substitute-if #\_ (complement #'alphanumericp) (prin1-to-string exp)))

(defparameter *max-label-length* 30)

(defun dot-label (exp)
  "Truncate exp to *max-label-length* characters."
  (if exp
    (let ((s (write-to-string exp :pretty nil)))
      (if (> (length s) *max-label-length*)
        (concatenate 'string (subseq s 0 (- *max-label-length* 1)) "…")
        s))
    ""))

(defparameter *wizard-nodes* '((living-room (you are in the living-room. a wizard is snoring loudly on the couch.))
                               (garden (you are in a beautiful garden. there is a well in front of you.))
                               (attic (you are in the attic. there is a giant welding torch in the corner.))))

(defparameter *wizard-edges* '((living-room (garden west door) (attic upstairs ladder))
                               (garden (living-room east door))
                               (attic (living-room downstairs ladder))))

(defun nodes->dot (nodes)
  "Convert a list of nodes to DOT format. Each node is a list with
  - title: symbol
  - description: list of symbols."
  (mapc (lambda (node)
          (fresh-line)
          (princ (dot-name (car node)))
          (princ "[label=\"")
          (princ (dot-label node))
          (princ "\"];"))
        nodes))


(defun edges->dot (edges)
  "Convert list of edges to DOT format. Each edge is a list with
  - source: symbol
  - list: (<destination> <cardinal direction> <means>)."
  (mapc (lambda (node)
          (let ((origin (car node)))
            (mapc (lambda (edge)
                    (let ((dest (car edge))
                          (rest (cdr edge)))
                      (fresh-line)
                      (princ (dot-name origin))
                      (princ "->")
                      (princ (dot-name dest))
                      (princ "[label=\"")
                      (princ (dot-label rest))
                      (princ "\"];")
                      ))
                  (cdr node))))
        edges))

(defun graph->dot (nodes edges)
  (princ "digraph {")
  (nodes->dot nodes)
  (edges->dot edges)
  (princ "}"))

(defun dot->png (fname thunk)
  (with-open-file (*standard-output*
                    fname
                    :direction :output
                    :if-exists :supersede)
    (funcall thunk))
  (uiop:run-program (format nil "dot -Tpng -O ~A" fname)))

(defun graph->png (fname nodes edges)
  (dot->png fname
            (lambda ()
              (graph->dot nodes edges))))

(graph->png "graph.dot" *wizard-nodes* *wizard-edges*)

(defun uedges->dot (edges)
  (maplist (lambda (lst)
             (mapc (lambda (edge)
                     (unless (assoc (car edge) (cdr lst))
                       (fresh-line)
                       (princ (dot-name (caar lst)))
                       (princ "--")
                       (princ (dot-name (car edge)))
                       (princ "[label=\"")
                       (princ (dot-label (cdr edge)))
                       (princ "\"];")))
                   (cdar lst)))
           edges))

(defun ugraph->dot (nodes edges)
  (princ "graph{")
  (nodes->dot nodes)
  (uedges->dot edges)
  (princ "}"))

(defun ugraph->png (fname nodes edges)
  (dot->png fname
            (lambda ()
              (ugraph->dot nodes edges))))

