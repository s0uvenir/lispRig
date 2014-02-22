;;; Basic PSO Functions ;;;
(defparameter gbest-f nil)            ;Global Best Fitness
(defparameter gbest-p nil)
(defparameter *swarm-dump* nil)

(defclass particle ()
  ((position  :initform (vector)      ;Particle Position
	      :initarg  :position
	      :reader   see-position
	      :accessor set-position)   
   (pbest     :initform (vector)      ;Particles Personal Best Position
              :initarg  :pbest
              :reader   see-pbest)    
   (velocity  :initform 0             ;Particles Current Velocity
	      :initarg  :velocity
	      :reader   see-velocity
	      :accessor set-velocity)
   (fitness   :initform 0             ;Particles Fitness level (score)
	      :initarg  :fitness
	      :reader   see-fitness)
   (fbest     :initform (vector)      ;Best Personal Fitness Level  
	      :initarg  :fbest
	      :reader   see-fbest)))

;;; Algorithm ;;;

(defun pso1 (m &optional (population 10) (itr 10)
	     (w  .3) ;Intertia.              Typically 0 =< w  =< 1.2
	     (c1 2.1) ;Cognitive Coefficient. Typically 0 =< c1 =< 2
	     (c2 1.3));Social Coefficient.    Typically 0 =< c2 =< 2
  (let* ((model-pop (initialize m population))
	 (swarm)
	 (temp-swarm))
    (setf swarm (particle-sort (build-swarm model-pop))) ;Initialize Swarm
    (setf gbest-f (svref swarm 0)) ;Particle w/Initial Global Best Fitness
    (setf gbest-p (svref swarm 0)) ;Particle w/Initial Global Best Position
    (loop for particle across swarm do
	 (unless (eq particle gbest-f)
	   (progn (new-velocity w c1 c2 particle gbest-p 1)
		  (new-position particle)
		  (push particle temp-swarm))))
    (setf swarm (coerce temp-swarm 'vector))
    (particle-sort swarm)))

    

	   

;;; Initialization ;;;

(defun initialize (m &optional (population 10))
  "Builds initial population of models"
  (let* ((temp-pop))
    (loop for i from 1 to population do
	 (decisions! m)
	 (let* ((new (objectives! m)))
	   (push new temp-pop)))
   (coerce temp-pop 'vector)))


(defun build-swarm (shell-vec &optional vhigh)
  "Converts the model population to a population of scored particles"
  (let* ((temp-pop)
	 (vhigh  1)        ;particles initial lower velocity bound
	 (vlow (- 0 vhigh)));particles initial high  velocity bound
    (loop for i from 0 to (- (length shell-vec) 1) do
	 (push (make-instance 'particle
			      :position (calc-position (svref shell-vec i))
			      :pbest    (calc-position (svref shell-vec i))
			      :velocity (get-velocity vlow vhigh)
			      :fitness  (s-score (svref shell-vec i))
			      :fbest    (s-score (svref shell-vec i))) ;initial pbest is its initial fitness level
	       temp-pop))
    (coerce temp-pop 'vector)))

;;; Velocity Funcs ;;;

(defun new-velocity (w c1 c2 particle gbest-p dimension)
  (let* ((r1 (randi 1))
	 (r2 (randi 1)))
  (setf (set-velocity particle)
	(+ (* w (see-velocity particle))
	   (* (* c1 r1)
	      (- (svref (see-pbest particle) (- dimension 1)) 
		 (svref (see-position particle) (- dimension 1))))
	   (* (* c2 r2)
	      (- (svref (see-position gbest-p) (- dimension 1)) 
		 (svref (see-position particle) (- dimension 1))))))))

(defun get-velocity (low high)
  "Generates a random velocity"
  (let* ((vel (make-num :name 'x  :low low :high high)))
    (randgen vel)))

;;; Position Funcs ;;;

(defun new-position (particle)
  (unless (eq particle gbest-f)
    (progn (loop for i from 0 to (- (length (see-position particle)) 1) do
		(let* ((num1 (svref (see-position particle) i))
		       (num2 (svref (see-position gbest-p) i)))
		  (setf (svref (see-position particle) i)
			(* (see-velocity particle)
			   (- num1 num2)))))
	   particle)))

  
  
	  
	

(defun calc-position (shell)
  (let* ((x (coerce (s-decisions shell) 'vector)))
    x))

;;; Sorter ;;;

(defun particle-sort (particle-vector &optional (op #'>))
  (sort particle-vector op :key 'see-fitness))

