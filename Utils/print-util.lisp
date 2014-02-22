(defparameter *generations* nil)

(defmethod sexy-print ((s shell))
  "Takes an ugly shell and makes it sexy"
  (with-slots (id decisions objectives score) s
    (format t "~%~%ID: ~a~%Decisions: ~a~%Objectives: ~a~%**Score: ~a~%" id decisions objectives score)))

(defmethod sexy-print ((m model))
  "Takes an ugly model and makes it sexy"
  (with-slots (name inputs) m
    (format t "~%Name: ~a~%Inputs: ~a~%~%" name inputs)))

(defmethod print-shell-list ((lst list))
  "Takes a list of Shells and Prints them in a readable way"
  (dotimes (i (length lst))
    (sexy-print (nth i lst))))

(defun write-shell (lst filename)
  "takes a list of shell-objects and writes a csv"
  (with-open-file (phil filename :direction :output :if-exists :supersede)
    (loop for i from 1 to (length (s-decisions (car lst))) do
	 (format phil "Decision~a, " i))
    (loop for i from 1 to (length (s-objectives (car lst))) do
	 (format phil "Objective~a, " i))
    (format phil "Score~%")
    (loop for egg in lst do
	 (progn (loop for obj in (s-decisions egg) do
		     (format phil "~8f, " obj))
		(loop for obj in (s-objectives egg) do
		     (format phil "~8f, " obj))
		(format phil "~8f~%" (s-score egg))))))

(defun write-plt (lst filename chart-type)
  (with-open-file (phil filename :direction :output :if-exists :supersede)
    (format phil "~a" chart-type)
    (loop for item in lst do (progn
			       (format phil "~%~8f" (car item))
			       (loop for obj in (cdr item) do
				    (format phil ", ~8f" obj))))))

(defun gen-plot (gens filename &key (objective 0) (title nil))
  (let ((out))
    (labels ((getter (of_x)
	       (if objective
		   (nth objective (s-objectives of_x))
		   (s-score of_x))))
      (labels ((getterz (lst) 
		 (mapcar #'getter lst)))
	(setf out (mapcar #'getterz gens))))
    (if title
	(let ((line (concatenate 'string "genplot, " title)))
	  (write-plt out filename line))
	(write-plt out filename "genplot"))))

(defun load_funs ()
 "loads comparison functions operating on model, pop, and gens"
  (defun de-fun (model pop gens)
    (setf *generations* nil)
    (de model pop gens)
    (car (last *generations*)))

  (defun de2-fun (model pop gens)
    (setf *generations* nil)
    (de2 model pop gens)
    (car (last *generations*)))

  (defun ga-fun (model pop gens)
    (setf *generations* nil)
    (ga model :total-pop pop :num-gens gens)
    (car (last *generations*)))

  (defun sa-fun (model pop gens)
    (setf *generations* nil)
    (sa model :kmax gens)
    (let ((generations (mapcar 'list *generations*)))
      (loop for i from 0 to (1- pop) do
	   (sa model :kmax gens)
	   (setf generations (mapcar 'append generations (mapcar 'list *generations*))))
      (setf *generations* generations))
    (car (last *generations*))))

(defun plot_batch (&key (pop_size 100) (gens 100) (models '(kursawe viennet2)) (functions '((sa-fun "SA") (de-fun "DE"))))
  (setf models (mapcar 'make-instance models))
  (loop for func in functions do
       (loop for model in models do
	    (progn
	      (setf *generations* nil)
	      (format t "~a ~a...~%" (cadr func) (m-name model))
	      (funcall (car func) model pop_size gens)
	      (gen-plot *generations* (format nil "../Plots/~a_~a_score.plt" (cadr func) (m-name model))
			:objective  nil
			:title      (format nil "~a Pop: ~a Model: ~a Overall Score" (cadr func) pop_size (m-name model)))
	      (loop for obj from 0 to (1- (length (s-objectives (caar *generations*)))) do
		   (let ((title (format nil "~a Pop: ~a Model: ~a Objective: ~a" (cadr func) pop_size (m-name model) obj))
			 (fname (format nil "../Plots/~a_~a_~a.plt" (cadr func) (m-name model) obj)))
		     (gen-plot *generations* fname :objective obj :title title)
		     (format t "~a~%" fname)))
	      ))))
  




