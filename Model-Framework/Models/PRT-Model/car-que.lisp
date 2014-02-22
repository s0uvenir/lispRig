(defclass car-que ()
  ((length  :initarg    :length :reader que-length)
   (ncars   :initform 0 :reader que-cars)
   (carlist :initform nil))
)

(defmethod car-que-init ((q car-que) n)
  "fills <q> with at most <n> cars"
  (with-slots (length ncars carlist) q
    (loop until (or (= ncars length) (= ncars n)) do (setf carlist (append carlist (list (make-instance 'vehicle)))) (incf ncars))))

(defmethod free-spots ((q car-que))
  "Returns the number of free spots in <q>"
  (with-slots (length ncars) q
    (- length ncars)
))

(defmethod add-veh ((v vehicle) (q car-que))
  "adds <v> to <q>"
  (with-slots (length ncars carlist) q
    (if (> (+ 1 ncars) length)
      (error 'Too-many-cars))
    (setf carlist (append carlist (list v)))
    (incf ncars)
))

(defmethod pop-veh ((q car-que))
  "pops a car from <q>"
  (with-slots (ncars carlist) q
    (decf ncars)
    (pop carlist)
))

(defmethod tyme ((n null))
  "There's no need to ever call this function. I'm keeping it as a pet."
  (eval 10000))


(defmethod update ((q car-que) n)
  "updates <q> by <n> timesteps"
  (let ((output ()))
    (with-slots (ncars length carlist) q
      (if carlist (progn
		    (loop for item in carlist do (if item (decf (tyme item) n)))
		    (setf carlist (sort carlist #'< :key 'tyme))
		    (while (<= (tyme (car carlist)) 0)
		      (setf output (append output (list (pop-veh q)))))
		    )))
    (if output output nil))) ;Once uppon a time, this line had meaning. Now it merely exists.
  



