;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Do not mess with the following.

(let (files)
  (defun make  (&optional verbosep &rest new )
    (labels ((make0 (x)
	       (format t ";~a~%" x) (load x)))
      (if new
	  (setf files new))
      (if verbosep
	  (mapcar #'make0 files)
	  (handler-bind ; SBCL-specific
	      ((style-warning #'muffle-warning)) 
	    (mapcar #'make0 files)))
      (terpri)
      t))
  )


(make nil
      "../../lib/deftest.lisp" ; should be first 
      "../../lib/misc.lisp"
      "../../lib/macros.lisp"
      "../../lib/random.lisp"
      "../../lib/watch.lisp"
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
					; Add you local code here

      "shell.lisp"
      "Models/example-model.lisp"
      "Models/schaffer.lisp"
      "Models/fonseca.lisp"
      "Models/fonseca_max.lisp"
      "Models/viennet2.lisp"
      "Models/kursawe.lisp"
      "Models/zdt1.lisp"
      "Models/constr_ex.lisp"
      "Models/PRT-Model/person.lisp"
      "Models/PRT-Model/person-que.lisp"
      "Models/PRT-Model/vehicle.lisp"
      "Models/PRT-Model/car-que.lisp"
      "Models/PRT-Model/station.lisp"
      "Models/PRT-Model/model.lisp"
      "Models/PRT-Model/wrapper.lisp"
      "../Utils/print-util.lisp"
      "../Utils/stats-util.lisp"
      "../Utils/randgen.lisp"
      "../Utils/accessors.lisp"
      "../Utils/sorters.lisp"
      "../Algorithms/sa.lisp"
      "../Algorithms/ga.lisp"
      "../Algorithms/de.lisp"
      "../Algorithms/de2.lisp"
      "../Algorithms/pso1.lisp"
      "../Algorithms/brute_force.lisp"
      "../Stats/a12.lisp"
   
      

      )
(load_funs)
(defparameter *generations* nil)

