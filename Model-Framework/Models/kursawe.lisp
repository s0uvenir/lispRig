(defclass kursawe (model)
  ((name   :initform "Kursawe")
   (inputs :initform (list (make-num :name 'x1 :low -5 :high 5)
			   (make-num :name 'x2 :low -5 :high 5)
			   (make-num :name 'x3 :low -5 :high 5)))
   (eqs    :initform (list (lambda (inputs &optional (a 0.8) (b 3)) (let 
									((x0 (car inputs)) (x1 (nth 1 inputs)) (x2 (nth 2 inputs)))
								      (+ (* -10 (exp (* -0.2 (sqrt (+ (expt x0 2) (expt x1 2))))))
									 (* -10 (exp (* -0.2 (sqrt (+ (expt x1 2) (expt x2 2)))))))))
			   (lambda (inputs &optional (a 0.8) (b 3)) (let 
									((x0 (car inputs)) (x1 (nth 1 inputs)) (x2 (nth 2 inputs)))
								      (+ (+ (expt (abs x0) a) 
									    (* 5 (expt (sin x1) b)))
									 (+ (expt (abs x1) a) 
									    (* 5 (expt (sin x2) b))))))
			   ))
   (a     :initform nil
	  :initarg  :a)
   (b     :initform nil
	  :initarg  :b)
   
   ))

(defmethod objectives! ((m kursawe))
  (with-slots (eqs inputs a b) m
    (let ((inlist (mapcar #'v inputs)) outlist)
      (if (and a b)
	  (setf inlist (append inlist (list a b))))
      (loop for lamb in eqs do
	   (setf outlist (append outlist (list (funcall lamb inlist)))))
      (setf outlist (kurs-scale outlist))
      (make-instance 'shell
		     :model m
		     :score (geosum outlist)
		     :decisions (mapcar #'v inputs)
		     :objectives outlist)
      )))

(defun kurs-scale (lst)
  (setf (car lst) (/ (+ 20 (car lst)) 16))
  (setf (car (cdr lst)) (/ (+ 9 (car (cdr lst))) 27))
  (append lst))


(defmethod mutate ((m kursawe) &optional (p 0.05))
  "Copies the old values over updating objectives"
  (let ((new (make-instance 'kursawe :inputs (read-inputs m))))
    (mutate (params new))
    new))