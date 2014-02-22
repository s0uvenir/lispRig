

(defun reset_prt ()
(setf (gethash 'walnut *stations*) 
             (make-instance 'station :north '(beechurst engineering towers med)
		                     :south '()
				     :name 'walnut
				     :n-turn (make-instance 'car-que :length 6)
				     :n-through (make-instance 'car-que :length 0)
				     :s-turn (make-instance 'car-que :length 0)
				     :s-through (make-instance 'car-que :length 0)
				     :distance 0))

(setf (gethash 'beechurst *stations*)
                (make-instance 'station :north '(engineering towers med)
			                :south '(walnut)
					:name 'beechurst
					:n-turn (make-instance 'car-que :length 12)
					:n-through (make-instance 'car-que :length 3)
					:s-turn (make-instance 'car-que :length 4)
					:s-through (make-instance 'car-que :length 3)
					:distance 120))

(setf (gethash 'engineering *stations*)
                  (make-instance 'station :north '(towers med)
			                  :south '(beechurst walnut)
					  :name 'engineering
					  :n-turn (make-instance 'car-que :length 4)
					  :n-through (make-instance 'car-que :length 3)
					  :s-turn (make-instance 'car-que :length 4)
					  :s-through (make-instance 'car-que :length 3)
					  :distance 400))

(setf (gethash 'towers *stations*)
             (make-instance 'station :north '(med)
  		                     :south '(engineering beechurst walnut)
				     :name 'towers
				     :n-turn (make-instance 'car-que :length 6)
				     :n-through (make-instance 'car-que :length 3)
				     :s-turn (make-instance 'car-que :length 6)
				     :s-through (make-instance 'car-que :length 3)
				     :distance 460))

(setf (gethash 'med *stations*)
          (make-instance 'station :north '()
			          :south '(towers engineering beechurst walnut)
				  :n-turn (make-instance 'car-que :length 0)
				  :name 'med
				  :n-through (make-instance 'car-que :length 0)
				  :s-turn (make-instance 'car-que :length 6)
				  :s-through (make-instance 'car-que :length 0)
				  :distance 550))

;Add cars to stations
(populate-station (gethash 'walnut      *stations*)  6)
(populate-station (gethash 'beechurst   *stations*)  6)
(populate-station (gethash 'engineering *stations*)  6)
(populate-station (gethash 'towers      *stations*)  6)
(populate-station (gethash 'med         *stations*)  6)




;Station and People shortcuts

(defparameter walnut      (gethash 'walnut      *stations*))
(defparameter beechurst   (gethash 'beechurst   *stations*))
(defparameter engineering (gethash 'engineering *stations*))
(defparameter towers      (gethash 'towers      *stations*))
(defparameter med         (gethash 'med         *stations*))
(defvar walnut_goer (make-instance 'person :dest 'walnut))
(defvar beechurst_goer (make-instance 'person :dest 'beechurst))
(defvar engineering_goer (make-instance 'person :dest 'engineering))
(defvar towers_goer (make-instance 'person :dest 'towers))
(defvar med_goer (make-instance 'person :dest 'med))

(defparameter *timestep* 0)
)

(defun update_all_stations ()
  (update beechurst 1)
  (update engineering 1)
  (update walnut 1)
  (update med 1)
  (update towers 1)
)

(defparameter *cached_instructions* nil)

(defun PRT_Main_Loop (&optional (quefile "out.q"))
  (reset_prt)
  (let ((line "stuff") (local1 nil) (local2 nil))
    (if (null *cached_instructions*) 
	(progn
	  (with-open-file (phil quefile)
	    (loop while (not (null line)) do
		 (setf line (read-line phil nil))
		 (push line local1)))
	  (setf line "stuff")
	  (pop local1)
	  (loop while (not (null line)) do
	       (setf line (pop local1))
	       (push line local2))
	  (loop while (null (car local2)) do
	       (pop local2))
	  (setf line "stuff")
	  (setf *cached_instructions* local2))
	(setf local2 *cached_instructions*))
    (loop while (not (null line)) do
	 (setf line (pop local2))
	 (incf *timestep*)
	 (if (or (string= line "") (null line))
	     nil 
	     (progn
	       (eval (read (make-string-input-stream line)))
	       (update_all_stations)
	       )))))


(reset_prt)