;;; Shell ;;;
(defun id-sort (lst)
  "Sorts Shells by the largest Model ID Value"
  (sort lst
	#'(lambda (first next)
	    (> (s-id first)
	       (s-id next)))))

(defun score-sort (lst)
  "Sorts Shells by the largest Model ID Value"
  (sort lst
	#'(lambda (first next)
	    (> (s-score first)
	       (s-score next)))))

(defun shell-sort (shell-list &optional (op #'<))
  (sort shell-list op :key 's-score))
