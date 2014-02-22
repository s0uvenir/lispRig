;;;For A12 File Generation;;;

(defun generate-treatment (model pop gens &key (filename "../Stats/results.lisp") objective)
  "Generates A12 Treatment File"
  (with-open-file (phil filename :direction :output :if-exists :supersede :if-does-not-exist :create)
    (let* (temp-sa temp-ga temp-de temp-de2 perc
	   (long-sa   (dotimes (_trash_ 1 (getgens)) (sa-fun   model pop gens)))
	   (long-ga   (dotimes (_trash_ 1 (getgens)) (ga-fun   model pop gens)))
	   (long-de   (dotimes (_trash_ 1 (getgens)) (de-fun   model pop gens)))
	   (long-de2  (dotimes (_trash_ 1 (getgens)) (de2-fun  model pop gens)))
	   (get_obj   (if objective #'(lambda (shell) (nth objective (s-objectives shell)))
			  #'(lambda (shell) (s-score shell)))))
      (format phil "(tiles '(~%")
	   (loop for genr from 1 to 1 do
		(setf temp-sa  (nth genr long-sa)
		      temp-ga  (nth genr long-ga)
		      temp-de  (nth genr long-de)
		      temp-de2 (nth genr long-de2)
		      perc     (getperc genr))
		(format phil "~%(SA-~a-~a~%" (m-name (s-id (car temp-sa))) perc);SA
		(loop for i from 1 to (length (s-objectives (car temp-sa))) do
		     (loop for obj in temp-sa do
			  (format phil "~8f " (funcall get_obj obj))))
		(format phil ")~%")
		(format phil "~%(GA-~a-~a~%" (m-name (s-id (car temp-ga))) perc);GA
		(loop for i from 1 to (length (s-objectives (car temp-ga))) do
		     (loop for obj in temp-ga do
			  (format phil "~8f " (funcall get_obj obj))))
		(format phil ")~%")
		(format phil "~%(DE-~a-~a~%" (m-name (s-id (car temp-de))) perc);DE
		(loop for i from 1 to (length (s-objectives (car temp-de))) do
		     (loop for obj in temp-de do
			  (format phil "~8f " (funcall get_obj obj))))
		(format phil ")~%")
		(format phil "~%(DE2-~a-~a~%" (m-name (s-id (car temp-de2))) perc);DE2
		(loop for i from 1 to (length (s-objectives (car temp-de2))) do
		     (loop for obj in temp-de2 do
			  (format phil "~8f " (funcall get_obj obj))))
		(format phil ")~%"))
	   (format phil "))~%")))
  )

(defun getperc (num)
  (cond 
    ((= num 0)  "1/8")
    ((= num 1)  "1/4")
    ((= num 2)  "1/2")
    ((= num 3)  "end")
    (t          "err")))

(defun getgens ()
  (let ((gens (1- (length *generations*))))
    (list (nth (floor (* gens 0.125)) *generations*)
	  (nth (floor (* gens 0.25 )) *generations*)
	  (nth (floor (* gens 0.5  )) *generations*)
	  (nth (floor (* gens 1    )) *generations*))))

(defun quick-a12 (model pop gens &optional obj)
  "Prints a12 Stats of a Model to REPL"
  (generate-treatment model pop gens :objective obj)
  (load "../Stats/results.lisp"))


(defun a12-tofile-dump ()
  (dribble "../2print/a12-results.txt")
  (format t "~%FONSECA~%")
  (quick-a12 (make-instance 'fonseca) 100 100)
  (format t "~%VIENNET2~%")
  (quick-a12 (make-instance 'viennet2) 100 100)
  (format t "~%KURSAWE~%")
  (quick-a12 (make-instance 'kursawe) 100 100)
  (format t "~%ZDT1~%")
  (quick-a12 (make-instance 'zdt1) 100 100)
  (format t "~%Constr-Ex~%")
  (quick-a12 (make-instance 'constr_ex) 100 100)
  (dribble))