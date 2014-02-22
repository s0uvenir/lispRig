(defclass zdt1 (model)
  ((name   :initform "ZDT-1")
   (inputs :initform (loop for i to 29 collecting 
			  (make-num :name (list 'x i) :low 0 :high 1)))     
			  
   (eqs    :initform (list (lambda (inputs)
			     (nth 0 inputs))
			   (lambda (inputs)
			     (*
			      (let ((i 1) (n 29)) ;from 2 to 30
				(+ 1
				   (* 9
				      (loop for i from i to n
					 summing (nth i inputs) into tmp
					 finally (return tmp))
				      (/ (- n 1)))))
			      (- 1 (sqrt (/ (nth 0 inputs) 
					    (let ((i 1) (n 29))
					      (+ 1
						 (* 9
						    (loop for i from i to n
						       summing (nth i inputs) into tmp
						       finally (return tmp))
						    (/ (- n 1)))))))))))))) ;This looks crazy but I'm pretty sure its right..
                                                                            ;each loop statement is g(x)

(defmethod objectives! ((m zdt1))
  (with-slots (eqs inputs a b) m
    (let ((inlist (mapcar #'v inputs)) outlist)
      (loop for lamb in eqs do
	   (setf outlist (append outlist (list (funcall lamb inlist)))))
      (setf outlist (zdt1-scale outlist))
      (make-instance 'shell
		     :model m
		     :score (geosum outlist)
		     :decisions (mapcar #'v inputs)
		     :objectives outlist))))

      
(defun zdt1-scale (lst)
  (setf (cadr lst) (/ (- (cadr lst) 2) 6))
  (append lst))
      

(defmethod mutate ((m zdt1) &optional (p 0.05))
  "Copies the old values over updating objectives"
  (let ((new (make-instance 'zdt1 :inputs (read-inputs m))))
    (mutate (params new))
    new))
  
  