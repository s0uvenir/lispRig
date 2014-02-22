(defmethod brute-force ((m model) &optional (num 100))
  (let (shell-list filename)
    (loop for i from 1 to num do
	 (push (decisions! m) shell-list))
    (format t "Sorting...~%")
    (princ (length shell-list))
    (setf shell-list (shell-sort shell-list))
    (princ (length shell-list))
    (setf filename (format nil "~a_brute-force_~a.csv" (slot-value m 'name) num))
    (format t "Writing...~%")
    (write-shell shell-list filename)
))
    