(defclass person ()
  ((enterTime :initform 0 :initarg :t :accessor enterTime)
   (destination :initarg :dest))
)

(defmethod clone ((p person))
  (with-slots (destination) p
    (make-instance 'person :t *timestep* :dest destination)))

(defmethod waittime ((p person))
  (with-slots (enterTime) p
    (- *timestep* enterTime)))

