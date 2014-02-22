(defclass fonseca_max (model)
  ((name   :initform "fonseca_max")
   (inputs :initform (list (make-num :name 'x  :low -4 :high 4)
			   (make-num :name 'x2 :low -4 :high 4)
			   (make-num :name 'x3 :low -4 :high 4)))
   (eqs    :initform (list (lambda (inputs) 
			     (let ((result 0))
			       (exp (- (dotimes (count (length inputs) result)
					 (incf result (expt (- (nth count inputs) (/ 1 (sqrt 3))) 2)))))))
			   (lambda (inputs) 
			     (let ((result 0))
			       (exp (- (dotimes (count (length inputs) result)
					 (incf result (expt (+ (nth count inputs) (/ 1 (sqrt 3))) 2)))))))))))
			   

(defmethod objectives! ((m fonseca_max))
  (with-slots (eqs inputs) m
    (let ((inlist (mapcar 'v inputs)) outlist)
      (loop for lamb in eqs do
	   (setf outlist (append outlist (list (funcall lamb inlist)))))
      (make-instance 'shell
		     :model m
		     :score (geosum outlist)
		     :decisions inlist
		     :objectives outlist))))


(defmethod mutate ((m fonseca_max) &optional (p 0.05))
  "Copies the old values over updating objectives"
  (let ((new (make-instance 'fonseca_max :inputs (read-inputs m))))
    (mutate (params new))
    new))
