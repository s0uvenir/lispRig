;;;Accessors for Inputs;;;

(defun see-inputs (s)
  "Taking a peek into the model to see the inputs"
  (read-inputs (s-id s)))

(defun set-inputs (s val)  
  "Writes new inputs into the model"
  (write-inputs val (s-id s)))


;;;Updator for Inputs;;;

(defmethod update-decisions ((s shell))
  "Only updates the shells decisions"
  (setf (s-decisions s) (settings (s-id s))))

(defmethod update-objectives ((s shell))
  "Only updates the shells objectives"
  (setf (s-objectives s) (s-objectives (objectives! (s-id s)))))

(defmethod update-score ((s shell))
  "Only updates the shells score"
  (setf (s-score s) (s-score (objectives! (s-id s)))))

(defmethod update-shell ((s shell))
  "Updates the shell"
  (update-decisions s)
  ;(update-objectives s)
  ;(update-score s)
  (objectives! (s-id s)))

;;;Accessors for NUM Values (ie. x1 x2 x3);;;

(defun see-xs (s &optional (n 0))
  "Taking a peek into the model to see the X value at N"
  (stuff-value (nth n (read-inputs (s-id s)))))

(defun get-xs (s &optional (n 0) (val 0))
  "The function set-xs expands into"
  (setf (stuff-value (nth n (m-inputs (s-id s)))) val))

(defsetf set-xs get-xs)  ;Sets NUM X values



;;;;;;;Tests;;;;;;;

(defun _xs-test ()
  (let ((testObj))
    (setf testObj (decisions! (make-instance 'specific-model)))
    (format t "~%~%***Fetching X values***~%X1: ~a~%X2: ~a~%~%"
	    (see-xs testObj 0) 
	    (see-xs testObj 1))
    (describe testObj)
    (format t "~%~%***Setting NEW X values***~%")
    (setf (set-xs testObj 0) 1)
    (setf (set-xs testObj 1) 2)
    (format t "~%~%***Fetching NEW X values***~%X1: ~a~%X2: ~a~%~%"
	    (see-xs testObj 0) ;;Notice how these dont reflect back up to
	    (see-xs testObj 1));;the shell quite yet until...
    (describe testObj)
    (update-shell testObj)
    (format t "~%~%***Updating the Shell***~%~%")
    (describe testObj)))

    