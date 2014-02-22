(defclass schaffer (model)
  ((name   :initform "Schaffer")
   (inputs :initform (list (make-num :name 'x  :low -100000 :high 100000)))
   (eqs    :initform (list (lambda (inputs) 
			     (/ (expt (nth 0 inputs) 2) 10000000000))
			   (lambda (inputs) 
			     (/ (expt (- (nth 0 inputs) 2) 2) 10000000000))))))
	   

(defmethod objectives! ((m schaffer))
  (with-slots (eqs inputs) m
    (let ((inlist (mapcar 'v inputs)) outlist)
      (loop for lamb in eqs do
	   (setf outlist (append outlist (list (funcall lamb inlist)))))
      (make-instance 'shell
		     :model m
		     :score (geosum outlist)
		     :decisions inlist
		     :objectives outlist))))


(defmethod mutate ((m schaffer) &optional (p 0.05))
  "Copies the old values over updating objectives"
  (let ((new (make-instance 'schaffer :inputs (read-inputs m))))
    (mutate (params new))
    new))
