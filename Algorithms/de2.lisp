;;; Differential Evolution Functions ;;;


(defun random-elt (choices);;; Coppied from Norvig
  "Choose an element from a list at random."
  (elt choices (random (length choices))))

(defun de2 (m &optional (n 100) (g 100))
  "DE with NSGA-II Pruning"
  (let* ((population (generate m n))) ;initialize population
    (dotimes (i g population)
      (let* ((f                                     .7)
	     (cr                                    .5)
	     (zero)  (one)  (two)  (tmp)  (new) (mutants))
	(loop for old in population do
	     (setf zero        (random-elt population))
	     (setf one         (random-elt population))
	     (setf two         (random-elt population))
	     (setf tmp         (extrapolate m zero one two f)) ;extrapolate
	     (setf new         (cross-over cr old tmp))   ;crossover	 
	     (push new mutants))
	(setf population (de-replace population mutants))
	(setf population (prune  population n))
	(setf *generations* (append *generations* (list population)))))))

(defun de-replace (one two)
  (let ((new)) 
    (loop while t do
	 (cond
	   ((not (car one))                             ; one is empty
	    (return-from de-replace new))
	   ((not (car two))                             ; two is empty
	    (push (pop one) new))
	   ((domination (car one) (car two))            ; one doimnates two
	    (progn (push (pop one) new) (pop two)))
	   ((domination (car two) (car one))            ; two dominates one
	    (progn (push (pop two) new) (pop one)))
	   (t                                         ; neither dominates
	    (progn (push (pop one) new) (push (pop two) new)))))))

(defun prune (shell_list outsize)
  (loop for item in shell_list do
       (setf (s-score item) 0)) ; using s-score as temporary crowding distance storage
  (loop for i from 0 to (1- (length (s-objectives (car shell_list)))) do ;itterates over objectives
       (setf shell_list (sort shell_list '< :key (lambda (a) (nth i (s-objectives a)))))
       (let ((current shell_list))
	 (loop for j from 1 to (- (length shell_list) 2) do
	      (incf (s-score (cadr current)) (- (nth i (s-objectives (caddr current)))
						(nth i (s-objectives (car current)))))
	      (setf current (cdr current)))))
  (setf shell_list (sort shell_list '< :key 's-score))
  (mapcar 'update-score shell_list) ; return s-score
  (subseq shell_list 0 outsize))


(defun domination-prune (shell_list outsize)
  (let ((outlist) (current_rank))
    (loop while (and (not (null shell_list)) (> outsize 0)) do (progn
	 (loop for item in shell_list do
	      (if (notany (lambda (a) (domination a item)) shell_list) (push item current_rank)))
	 (loop for item in current_rank do
	      (setf shell_list (remove item shell_list)))
	 (if (<= (length current_rank) outsize)
	     (progn
	       (setf outlist (append outlist current_rank))
	       (decf outsize (length current_rank)))
	     (progn
	       (setf current_rank (sort current_rank '< :key 's-score))
	       (loop while (> outsize 0) do (progn
		      (push (pop current_rank) outlist)
		      (decf outsize)))))
	 (setf current_rank nil)))
    (setf outlist outlist)))
	 
	   


	 
	 
  
