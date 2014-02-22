(defmacro oo (&rest l)
  "Print a symbol and its binding."
  `(progn (terpri) (o ,@l)))

(defmacro o (&rest l)
  "Print a list of symbols and their bindings."
  (let ((last (gensym)))
    `(let (,last)
       ,@(mapcar #'(lambda(x) `(setf ,last (oprim ,x))) l)
       (terpri)
       ,last)))

(defmacro oprim (x)
  "Print a thing and its binding, then return thing."
  `(progn (format t "~&[~a]=[~a] " (quote ,x) ,x) ,x))

(defmacro doitems ((one n list &optional out) &body body )
  "Set 'one' and 'n' to each item in a list, and its position."
  `(let ((,n -1))
     (dolist (,one ,list ,out)
       (incf ,n)
       ,@body)))

(defmacro dohash ((key value hash &optional end) &body body)
  "Iterate through all keys and values in a hash."
  `(progn (maphash #'(lambda (,key ,value)
		              ,@body)
		      ,hash)
	    ,end))

(defmacro while (test &rest body)
  `(loop (unless ,test (return nil))
      ,@body))
