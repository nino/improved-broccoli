(defun hashtbl-from-alist (alist)
  (let ((table (make-hash-table)))
    (dolist (pair alist)
      (let ((key (car pair))
            (value (cadr pair)))
        (setf (gethash key table) value)))
    table))
