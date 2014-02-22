					; assumes minimization
(defun prob (old new temp)
  (expt 2.718281828  (/ (- old new) temp)))

(defun energy (news olds)
  "Assumes positive numbers"
  (if (> (reduce #'* olds) 0)
      (labels ((sq (x) (* x x )))
	(let* ((norms (mapcar #'/ news olds))
	       (sqs   (mapcar #'sq norms))
	       (sums  (reduce #'+ sqs))
	       (out  (/ (sqrt sums)
			(sqrt (length news)))))
	  out))
      0))

(defun sa (m &key 
	   (p    1)
	   ;(emax 0.01)
	   (kmax 100) 
	   (omaxs (objs m))
	   (debug 10))
  "assumes goal is to minimize"
  (let* ((s     m)
	 (e    (energy omaxs omaxs))
	 (sbest s)
	 (ebest e)
	 (k     1)
	 (debug1 1))
    (setf *generations* nil)
    ;(while (and (< k kmax) (> e emax))
    (while (< k kmax)
      (let* ((temp (/ k kmax))
	     (snew (mutate s p))
	     (enew (energy (objs snew) omaxs)))
	;(princ ".")
	(when (> (prob e enew temp) (randf))
	  (setf s snew
		e enew))
	(when  (< enew ebest)
	  (setf sbest snew
		ebest enew))
	(incf k)
	(when (zerop (decf debug1))
	  ;(format t "~%> ~a~%" ebest)
	  (setf debug1 debug)))
      (setf *generations* (append *generations* (list (objectives! sbest)))))
    ;(sexy-print sbest)
    sbest))

;;;;;;;;;;;
(defun objs (m &optional (update t))
  (if update
      (s-objectives (decisions! m))
      (s-objectives (objectives! m))))

;;;;;;;;;;;Tester

(defun _sa (&key (p (/ 1 (sqrt 2))) (debug 10) (emax .001) (kmax 100) (seed 1))
  (reset-seed seed)
  (sa (make-instance 'example-model)
      :p p :debug debug :emax emax :kmax kmax))



;;;;;;;;;Shell Functions

(defun expand-sa (model)
  (objectives! model))