;;;;;Random number and list generation

;;;;;Some containers to hold values in case we need them

(defstruct stuff
  name
  value)

(defun v (x)
  "Extracts the value of x from the stuff structure"
  (stuff-value x))

(defstruct (num (:include stuff))
  "Defines a basic number structure"
  (low 0)
  (high 100))

;;;;These are the ones you run

(defmethod randgen ((lst list))
  "Picks randomly from a list"
  (nth (randi (length lst)) lst))

(defmethod randgen ((n number))
  "Generates a number from 0 to n"
  (randgen (make-num :low 0 :high n)))

(defmethod randgen ((n num))
  "Generates a number between the keys :high :low"
  (with-slots (low high)
    n
    (+ low (randf (- high low)))))

;;;;Methods that apply randgen to things

(defmethod randgen! ((x stuff))
  "Randomly generates a value inside the stuff structure"
  (setf (stuff-value x)
	(randgen x)))

(defmethod randgen! ((m model))
  "Applies randgen to the values inside a list of Parameters"
  (mapcar #'randgen! (params m)))
