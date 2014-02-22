(defun f (n &optional (w 5) (d 2))
  (let ((str (format nil "~~~s,~sF" w d)))
    (print str)
    (format nil str n)))

(defmethod print-object ((h hash-table) str)
  "Change the print method for a hash."
  (format str "{hash of ~a items}" (hash-table-count h)))

(defun sym-sorter()
  #'(lambda (x y)
      (string< (symbol-name x)
	       (symbol-name y))))
