#|
Motivation
==========

I don't like the standard way you have to define a class in CLOS. For example:
|#

 (defun  how-i-dont-want-to-define-classes ()
  (defclass fourth-grader () 
    ((teacher :initarg :teacher 
              :initform "Mrs. Marple"
              :accessor fourth-grade-teacher)
     (name :initarg :name
           :reader name)
     (school :initarg :school
             :initform "Lawrence School"
             :accessor school)
     (age :initarg :age
          :initform 9
          :accessor age))))

#|
I'd prefer something far less verbose, that internally compiles
the longer form. Also, I want to be able to get/set all slots without
all that 

     (setf (class-slot x) (+ (class-slot y) (class-slot z)

I want to be able to do

 (in someclass var
    (incf $x (+ $y $z)))

where `$x` is a way to reference an instance variable (which lets us
distinguish instance variables from everything else).
 
in
==

|#

(defun class-slot-names (class-name); warning: sbcl specific
 (mapcar #'sb-mop:slot-definition-name
         (sb-mop:class-slots (find-class class-name))))


(defmacro in (class var &rest body) 
  `(with-slots ,(class-slot-names class) ,var 
     ,@body))


#|

defklass
========

|#

(defmacro defklass (class parents &rest slots)
  (labels ((up (&rest things) ;list of things => 1 upcase string
	     (with-output-to-string (s)
	       (dolist (thing things)
		 (format s "~:@(~a~)" thing))))
	   (tweak (slot) ;make slot
	     (if (atom slot) ;list slots contain initforms
		 (defslot (up slot) nil ) 
		 (defslot (up (first slot)) (second slot))))
	   (defslot (slot init) ;finally, we can do the work
	     `(,(intern slot) 
		:initarg  ,(intern slot "KEYWORD")
		:initform ,init
		:accessor ,(intern (up class '- slot)))))
    `(defclass ,class ,parents ,(mapcar #'tweak slots))))

#|

Example
=======

pretty cool
-----------


In action

|#
(defklass 4grade ()
  $name
  ($age 10)
  ($teacher "msrs marple")
  ($school "lawrnce"))

(defklass 4gradea (4grade)
  ($n 20))

(deftest ?defklass () 
  (test 
   40
   (let ((x (make-instance '4gradea)))
     (in 4gradea x
	 (incf $n (+ 10 $age))
	 $n))))

#|

EMACS config
============

(add-hook 'lisp-mode-hook
  (lambda ()
    (font-lock-add-keywords nil
      '(("(\\(\\defklass\\)\\s \\(\\(?:\\s_\\|\\sw\\)+\\)"
         (1 font-lock-keyword-face)
         (2 font-lock-type-face))))))

(put 'defklass 'common-lisp-indent-function
     (get 'defclass 'common-lisp-indent-function))

|#
