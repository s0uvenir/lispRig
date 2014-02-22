(defclass PRT-wrapper (model)
  ((name   :initform "PRT")
   (inputs :initform (list (make-num :name 'ppcar  :low 15 :high 17) ;people per car
			   (make-num :name 'ttime  :low 60 :high 600) ;target wait time
			   (make-num :name 'cutoff :low 1  :high 10)));cutoff # of en-route cars
   (eqs    :initform nil)))
	   
(defmethod objectives! ((m prt-wrapper))
  (with-slots (inputs) m
    (let ((inlist (mapcar 'v inputs)) outlist)
      (setf *dead_people*                 nil)
      (setf *time_traveled*                 0)
      (setf *people_per_car*   (nth 0 inlist))
      (setf *target_wait_time* (nth 1 inlist))
      (setf *gimmie_cutoff*    (nth 2 inlist))
      (prt_main_loop)
      (setf outlist (list (/ (mean *dead_people*) 300)
			  (/ *time_traveled* 20000)))
			  
      (make-instance 'shell
		     :model m
		     :score (geosum outlist)
		     :decisions inlist
		     :objectives outlist))))


(defmethod mutate ((m prt-wrapper) &optional (p 0.05))
  "Copies the old values over updating objectives"
  (let ((new (make-instance 'prt-wrapper :inputs (read-inputs m))))
    (mutate (params new))
    new))

