;;;The Contents of this file **Don't Change**;;;

(defclass shell()
  ((id
    :initarg :model
    :accessor s-id)
   (decisions
    :initarg :decisions
    :accessor s-decisions)
   (objectives
    :initarg :objectives
    :accessor s-objectives)
   (score
    :initarg :score
    :accessor s-score)))

(defclass model ()
  ((name
    :initform "Default"
    :initarg  :name
    :accessor m-name
    :reader   read-name
    :writer   write-name)
   (inputs
    :initform '(0 0)		       
    :initarg  :inputs
    :accessor m-inputs
    :reader   read-inputs
    :writer   write-inputs)))


;;;Model Parameters list (values to be manipulated before evaluation);;;

(defmethod params ((m model))
  "Which inputs are we using?"
  (m-inputs m))

(defmethod settings (m)
  "When we want to extract values from the NUM structs"
  (mapcar #'v (params m)))

;;;Functions that manipulate the Model;;;

(defmethod decisions! ((m model))
  "Generates decisions for the model and then evaluates them"
  (randgen! m)
  (or (objectives! m)
      (decisions!  m)))

;;;Scoring;;;

(defun geosum (lst)
  (let ((squares (mapcar (lambda (x) (expt x 2)) lst)))
    (sqrt (apply '+ squares))))
  

;;;Mutator Examples;;;

(defmethod mutate ((lst list)  &optional (p 0.5))
  (dolist (one lst lst)
    (if (< (randf) p)
	(randgen! one))))

;;;Testing Functions;;;