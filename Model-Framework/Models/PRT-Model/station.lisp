(defparameter *stations* (make-hash-table))
(defparameter *dead_people* nil)
(defparameter *time_traveled* 0)
(defparameter *people_per_car* 16)
(defparameter *target_wait_time* (* 5 60))
(defparameter *gimmie_cutoff* 2)

(defclass station ()
  ((name       :initarg :name)
   (north      :initarg :north)
   (south      :initarg :south)
   (n-turn     :initarg :n-turn)
   (n-through  :initarg :n-through)
   (s-turn     :initarg :s-turn)
   (s-through  :initarg :s-through)
   (n-en-route :initform (make-instance 'car-que :length 50))
   (s-en-route :initform (make-instance 'car-que :length 50))
   (distance   :initarg :distance)
   (people     :initform (make-hash-table)))
)

(defmethod populate-station ((s station) n)
  "initializes and adds up-to n cars at each car-que of station s"
  (with-slots (n-turn s-turn n-through s-through north south people) s
    (car-que-init n-turn n)
    (car-que-init s-turn n)
    (car-que-init n-through n)
    (car-que-init s-through n)
    (loop for item in (append south north) do
	 (setf (gethash item people) (make-instance 'person-que)))
))

(defmethod print-station ((s station))
  "Prints info for Station s"
  (with-slots (name n-turn s-turn n-through s-through n-en-route s-en-route people south north) s
    (format t "~a:~%" name)
    (format t "n-turn: ~a/~a  " (que-cars n-turn) (que-length n-turn))
    (format t "n-thru: ~a/~a  " (que-cars n-through) (que-length n-through))
    (format t "s-turn: ~a/~a  " (que-cars s-turn) (que-length s-turn))
    (format t "s-thru: ~a/~a  " (que-cars s-through) (que-length s-through))
    (format t "n-en-route: ~a  " (que-cars n-en-route))
    (format t "s-en-route: ~a~%" (que-cars s-en-route))
    (loop for item in (append south north) do
	 (format t "~a: ~a|~a  " item (num_peeps (gethash item people)) (max_wait (gethash item people))))
    (format t "~%~%")
))

(defmethod omw-car ((s station) (veh vehicle) n dist)
  "<veh> is on it's way to <s> going direction <n> and will arrive in <dist> seconds."
  (if n; north
      (with-slots (s-turn n-through n-en-route s-en-route) s
	(if (= (+ (free-spots s-turn) (free-spots n-through)) 0)
	    (station-give-away s n))
	(setf (slot-value veh 'time_remaining) dist)
	(incf *time_traveled* dist)
	(add-veh veh n-en-route))
      (with-slots (n-turn s-through n-en-route s-en-route) s
	(if (= (+ (free-spots n-turn) (free-spots s-through)) 0)
	    (station-give-away s n))
	(setf (slot-value veh 'time_remaining) dist)
	(add-veh veh s-en-route))
))

(defmethod recieve-car ((s station) (veh vehicle) n)
  "<s> recieves <v> in the direction <n>"
  (if n; north
    (with-slots (s-turn n-through) s
      (if (> (+ (free-spots s-turn) (free-spots n-through)) 0)
        (if (> (free-spots s-turn) (free-spots n-through))
          (add-veh veh s-turn)
          (add-veh veh n-through)
	  )
	(progn (station-give-away s n)
	       (recieve-car s veh n))
      );if
    );with
    (with-slots (n-turn s-through) s
      (if (> (+ (free-spots n-turn) (free-spots s-through)) 0)
        (if (> (free-spots n-turn) (free-spots s-through))
          (add-veh veh n-turn)
          (add-veh veh s-through)
        )
	(progn (station-give-away s n)
	       (recieve-car s veh n))
        ;(format t "ERROR: No free spots at ~a~%" (slot-value s 'name))
      );if
    );with
  );if
);method

(defmethod give-car ((s station) n)
  "a car leaves from <s> in the direction <n>"
  (if n
    (with-slots (n-turn n-through) s
      (if (> (+ (que-cars n-turn) (que-cars n-through)) 0)
        (if (> (que-cars n-turn) (que-cars n-through))
          (pop-veh n-turn)
          (pop-veh n-through)
        );if
	(progn (gimmie s)
	       (eval nil)));if
    );with
    (with-slots (s-turn s-through) s
      (if (> (+ (que-cars s-turn) (que-cars s-through)) 0)
        (if (> (que-cars s-turn) (que-cars s-through))
          (pop-veh s-turn)
          (pop-veh s-through)
        );if
	(progn (gimmie s)
	       (eval nil)));if
    );with
))


(defmethod send-car ((s_from station) (s_to station))
  "sends a car from <s_from> to <s_to> including distance, en-route ques, the whole shebang."
  (let ((dist (abs (- (slot-value s_from 'distance) (slot-value s_to 'distance)))) veh)
    (if (member (slot-value s_to 'name) (slot-value s_from 'north))
	(progn ; going north
	  (setf veh (give-car s_from t))
	  (if (not (null veh)) 
	      (progn (omw-car s_to veh t dist)
		     (kill (gethash (slot-value s_to 'name) (slot-value s_from 'people)) *people_per_car*))    
	      (gimmie s_from);Request new car here
	      ))
	(progn ; going south
	  (setf veh (give-car s_from nil))
	  (if (not (null veh)) 
	      (progn (omw-car s_to veh nil dist)
		     (kill (gethash (slot-value s_to 'name) (slot-value s_from 'people)) *people_per_car*))     
	      (gimmie s_from);Request new car here
	      ))
)))
    
(defmethod gimmie ((s station))
  (with-slots (north south n-en-route s-en-route) s
    (if (< (+ (que-cars n-en-route) (que-cars s-en-route)) *gimmie_cutoff*)
	(let ((all (append north south)) mini)
	  (setf all (mapcar (lambda (x) (list (+ (can-accept (gethash x *stations*) t)
						 (can-accept (gethash x *stations*) nil)) x))
			    all))
	  (setf mini (cadr (assoc (apply 'min (mapcar 'car all)) all)))
	  (send-car (gethash mini *stations*) s)))))

(defmethod update ((s station) n)
  "updates a station n timesteps"
  (let ((northbound ()) (southbound ()))
    (with-slots (n-en-route s-en-route people) s
      (setf northbound (update n-en-route n))
      (setf southbound (update s-en-route n))
      (loop for name being the hash-key using (hash-value dest) of people do 
	   (progn (fast_update dest n)
		  (if (or (>  (max_wait  dest) *target_wait_time*)
			  (>= (num_peeps dest) *people_per_car*))
		      (send-car s (gethash name *stations*)))))      
      )
    ;Error Here
    (loop for item in northbound do (recieve-car s item t))
    (loop for item in southbound do (recieve-car s item nil))


))

(defmethod add-people ((s station) (p person) n)
  "Adds <n> clones of <p> to the proper ques in <s>"
  (with-slots (destination) p
    (with-slots (people) s
      (if (gethash destination people)
	  (progn (loop for _ from 1 to n do 
		      (add-person (gethash destination people) (clone p)))
		 (update (gethash destination people) 0))
	  (prin1 "ERROR: Bad Station"))
      )))

(defun can-accept (s n)
  (with-slots (n-turn n-through n-en-route s-turn s-through s-en-route) s
    (if n 
	(- (+ (free-spots s-turn) (free-spots n-through)) (que-cars n-en-route))
	(- (+ (free-spots n-turn) (free-spots s-through)) (que-cars s-en-route)))
	  ))

(defmethod station-give-away ((s station) n)
  "Gives a car away from station s in the n direction"
  (with-slots (north south) s
    (let ((possibilities) (finished))
      ;(if n (setf possibilities north) (setf possibilities south))
      (setf possibilities (remove nil (append north south)))
      (loop for dest in possibilities do
	   (if (not finished)
	       (if (> (can-accept (gethash dest *stations*) n) 0)
		   (progn (send-car s (gethash dest *stations*))
			  (setf finished t)))))
      (if (not finished)
	  (send-car s (gethash (car possibilities) *stations*))))))
			  
	       
	       