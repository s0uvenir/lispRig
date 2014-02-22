(defparameter *dead_people* '())

(defclass person-que ()
  ((total_wait   :initform 0 :accessor total_wait)
   (average_wait :initform 0 :accessor avg_wait)
   (max_wait     :initform 0 :accessor max_wait)
   (num_peeps    :initform 0 :accessor num_peeps)
   (people       :initform ())) 
)

(defmethod add-person ((q person-que) (p person))
  "Adds <p> to <q>"
  (with-slots (people num_peeps total_wait average_wait) q
    (setf people (append people (list p)))
    (incf num_peeps)
    (setf average_wait (/ total_wait num_peeps))))

(defmethod fast_update ((q person-que) n)
  "Updates <q> by <n> timesteps provided no more or less people"
  (with-slots (total_wait average_wait num_peeps max_wait) q
    (if (> num_peeps 0) (progn
			  (incf total_wait   (* num_peeps n))
			  (incf average_wait n)
			  (incf max_wait     n)))
    ))

(defmethod update ((q person-que) n)
  "Updates <q> by <n> timesteps"
  (with-slots (total_wait average_wait num_peeps people max_wait) q
    (setf total_wait 0 max_wait (if (null (car people)) 0 (waittime (car people))))
    (setf num_peeps (length people))
    (remove nil people)
    (loop for pers in people do
	 (incf total_wait (waittime pers)))
    (if (> num_peeps 0)(setf average_wait (/ total_wait num_peeps)) (setf average_wait 0))))

(defmethod kill ((q person-que) n)
  "Kill at most <n> people in <q>"
  (with-slots (people) q
    (let ((person))
    ;(setf people (sort people #'> :key 'waittime))
      (loop for _ from 1 to n do 
	 (setf person (pop people))
	   (if (not (null person)) 
	       (push (waittime person) *dead_people*)
		      ))))
				 
    (update q 0))
	 

