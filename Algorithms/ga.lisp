;;;;;;;;;;;;Genetic Algorithm Functions;;;;;;;;;;;;;;;;

(defun ga (m &key (kill-perc .8) (num-gens 10000) (total-pop 2) (p .1) (seed 1))
  (let* ((model          m)
	 (current-gen     )
	 (new-gen         ))
    (setf *generations* nil)
    (dotimes (i total-pop current-gen)      ;initializing the population
      (let ((new-model))
	(setf new-model (decisions! model))
	(push new-model current-gen)))
    (dotimes (gen num-gens current-gen) ;start of GA
      (if new-gen (setf current-gen new-gen))
      (loop for j in current-gen do
	   (mutate (s-id j)))
      (setf current-gen (ga-sort current-gen))
      (setf new-gen                       ; kills the pop
	    (subseq current-gen 0 (round (* kill-perc (length current-gen)))))
      (while (< (length new-gen) total-pop) ;builds pop back up (wip)
	(let ((mom)
	      (dad)
	      (child)
	      (crosspoint)
	      )
	  (setf mom (nth (randi (length current-gen)) current-gen))
	  (setf dad (nth (randi (length current-gen)) current-gen))
	  (setf crosspoint (randi (length (s-decisions mom))))
	  (setf child (objectives! m))
	  (setf (s-decisions child) (append 
				     (subseq (s-decisions mom) 0 crosspoint) 
				     (subseq (s-decisions dad) crosspoint)))
	  (update-shell child)
	  ;(setf child (cross-over 1 mom dad))
	  (push child new-gen) ;adds child to the list
	  ;(princ ".")
	  (setf new-gen (ga-sort new-gen))))
      (setf *generations* (append *generations* (list new-gen))))))


(defun ga-sort (lst)
  (sort lst #'> :key 's-score))

(defun _ga ()
  (ga (make-instance 'example-model)))

(defun __ga ()
  (let (scores horiz)
    (loop for i in '(10 20 30 40 50 60 70 80 90 100) do
	 (setf scores (append scores (list 
            (mapcar 's-score (ga (make-instance 'kursawe) :num-gens i :kill-perc 0.5 :total-pop 50)))))
	 )
    (write-plt scores "out.plt" "boxplot")
))
    
    