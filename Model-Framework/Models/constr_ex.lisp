(defclass constr_ex (model)
  ((name   :initform "Constr_Ex")
   (inputs :initform (list (make-num :name 'x  :low 0.1 :high 1)
			   (make-num :name 'x2 :low 0 :high 5)))
   (eqs    :initform (list (lambda (inputs) 
			     (nth 0 inputs))
			   (lambda (inputs) 
			     (/ (/ (1+ (nth 1 inputs)) (nth 0 inputs)) 10))))))
	   

(defmethod objectives! ((m constr_ex))
  (with-slots (eqs inputs) m
    (let ((inlist (mapcar 'v inputs)) outlist)
      (if (constr_ex_constraints inlist)
	  (loop for lamb in eqs do   ;constraints met
	       (setf outlist (append outlist (list (funcall lamb inlist)))))
	  (setf outlist '(0 0)))     ;constraints not met
      (make-instance 'shell
		     :model m
		     :score (geosum outlist)
		     :decisions inlist
		     :objectives outlist))))


(defmethod mutate ((m constr_ex) &optional (p 0.05))
  "Copies the old values over updating objectives"
  (let ((new (make-instance 'constr_ex :inputs (read-inputs m))))
    (mutate (params new))
    new))

(defun constr_ex_constraints (inputs)
  (and
   (>= (+ (* 9 (nth 0 inputs)) (nth 1 inputs)) 6)
   (>= (- (* 9 (nth 0 inputs)) (nth 1 inputs)) 1)))
  