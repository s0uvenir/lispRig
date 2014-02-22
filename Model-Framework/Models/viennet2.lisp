(defclass viennet2 (model)
  ((name   :initform "Viennet2")
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
	   

(defmethod objectives! ((m viennet2))
  (with-slots (eqs inputs) m
    (let ((inlist (mapcar 'v inputs)) outlist)
      (loop for lamb in eqs do
	   (setf outlist (append outlist (list (funcall lamb inlist)))))
      (setf outlist (v-scale outlist))
      (make-instance 'shell
		     :model m
		     :score (geosum outlist)
		     :decisions inlist
		     :objectives outlist))))


(defmethod mutate ((m viennet2) &optional (p 0.05)) ;;not working
  "Copies the old values over updating objectives"
  (let ((new (make-instance 'viennet2 :inputs (read-inputs m))))
    (mutate (params new))
    new))


(defun v-scale (objectives)
  (let ((zero (pop objectives))
	(one  (pop objectives))
	(two  (pop objectives)))
    (setf zero (/ (- zero 3) 20))
    (setf one (/ (+ one 18) 22))
    (setf two (/ (+ two 23) 11))
    (list zero one two)))