(defclass example-model (model)
  ((name   :initform "Example")
   (inputs :initform (list (make-num :name 'x  :low -4 :high 4)
			   (make-num :name 'x2 :low -4 :high 4)))
   (eqs    :initform (list (lambda (inputs) 
			     (+ (/ (expt (- (nth 0 inputs) 2) 2) 2)
				(/ (expt (+ (nth 0 inputs) 1) 2) 13)
				3.0))
			   (lambda (inputs) 
			     (- (+ (/ (expt (- (+ (nth 0 inputs) (nth 1 inputs)) 3) 2) 36)
				   (/ (expt (+ (- (nth 0 inputs)) (nth 1 inputs) 2) 2) 8))
				17))
			   (lambda (inputs) 
			     (- (+ (/ (expt (- (+ (nth 0 inputs) (* (nth 1 inputs) 2)) 1) 2) 175)
				   (/ (expt (+ (* (nth 1 inputs) 2) (nth 0 inputs)) 2) 17)
				   13)))))))
   


(defmethod objectives! ((m example-model))
  (with-slots (eqs inputs) m
    (let ((inlist (mapcar #'v inputs)) outlist)
      (loop for lamb in eqs do
	   (setf outlist (append outlist (list (funcall lamb inlist)))))
      (make-instance 'shell
		     :model m
		     :score (geosum outlist)
		     :decisions (mapcar #'v inputs)
		     :objectives outlist))))


(defmethod mutate ((m example-model) &optional (p 0.05))
  "Copies the old values over updating objectives"
  (let ((new (make-instance 'example-model :inputs (read-inputs m))))
    (mutate (params new))
    new))
  
  