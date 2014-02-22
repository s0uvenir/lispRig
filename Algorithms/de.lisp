;;; Differential Evolution Functions ;;;


(defun de (m &optional (n 100) (g 10))
  "Differential Evolution Algorithm"
  (let* ((population (generate m n))) ;initialize population
    (dotimes (i g population)
      (format t "Generation~a.............~%" i)	
      (let* ((f                                     .7)
	     (cr                                    .5)
	     (mutants) (old)  (one)  (two)  (three)  (tmp)  (new))
	(setf population  (shuffle population))   ;shuffle population
	(let ((matelist (append population (subseq population 0 4))))
	  (loop for dont_care from 1 to (length population) do
	       (setf old         (car     matelist))
	       (setf one         (cadr    matelist))
	       (setf two         (caddr   matelist))
	       (setf three       (cadddr  matelist))
	       (setf tmp         (extrapolate m one two three f)) ;extrapolate
	       (setf new         (cross-over cr old tmp))	;crossover
	       (setf mutants (append mutants (list new)))
	       (setf matelist (cdr matelist)))
	  (setf population (de-replace population mutants))
	  (setf population (domination-prune population n))
	  (setf *generations* (append *generations* (list population))))))))

(defun generate (m &optional (n 10))
  "Add new members of the population"
  (let ((total-population))
    (dotimes (i n total-population)
      (let* ((new (decisions! m)))
	(push new total-population)))))


(defun shuffle (lst)
  "Shuffles a list"
  (dotimes (i (1- (length lst)) lst)
    (rotatef
     (nth i lst)
     (nth (random (length lst)) lst))))


(defun domination (shell-one shell-two)
  "Checks for domination criteria then replaces or adds to the population"
  (let ((comp (mapcar '- (s-objectives shell-one) (s-objectives shell-two))))
    (if (and 
	 (= (count-if #'minusp comp) 0) ; worse on none
	 (> (count-if #'plusp comp) 0)) ; better on at least one
	t nil)))

(defun cross-over  (cr old tmp)
  "Chooses which of the old model's x-values gets transfered to the child"
  (let ((choice (random (length (m-inputs (s-id old))))))
    (if (< (randgen 1.0) cr)
	nil; This nil was originally (update-shell tmp)
	(setf (set-xs tmp choice) (see-xs old choice)))
    (update-shell tmp)))

 
(defun extrapolate (m one two three f)
  "Creates a child with x-values determined by three parents"
  (let ((tmp))
    (setf tmp (decisions! m)); i tried seting this to one, but it crapped itself. Perhaps we can rewrite with less decisions! and objectives!
    (dotimes (i (length (m-inputs m)))
      (setf (set-xs tmp i) (+ (see-xs one i)
			      (* f           
				 (- (see-xs   two i)
				    (see-xs three i))))))
    (update-shell tmp)
))
