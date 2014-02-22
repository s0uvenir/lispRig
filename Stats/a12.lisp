#|
Tim Menzies, 2013, http://menzies.us

A12
===

The Vargha and Delaney’s A12 statistics is a non-parametric effect
size measure.  Given a performance measure M seen in m measures of X
and n measures of Y, the A12 statistics measures the probability that
running algorithm X yields higher M values than running another
algorithm Y.

A12 = #(X >  Y)/mn + 0.5*#(X=Y)/mn

A. Vargha and H. D. Delaney. A critique and improvement of the CL
common language effect size statistics of McGraw and Wong. Journal of
Educational and Behavioral Statistics, 25(2):101–132, 200
rA12
====

A12 studies two treatments. rA12 handles mulitple treatments using a
Scott Knott procedure; i.e.  divide a list of treatments into two
lists _L1_ and _L2_ by finding the division that maximizes the
expected value of the sum of square of the mean differences before and
after divisions; i.e.

|L1| ∗ abs(L1.μ − L.μ)^2 + |L2| ∗ abs(L2.μ − L.μ)^2

rA12 calles itself recursively on the lsts terminating when no further
_useful_ division can be found (and _useful_ is checked by the A12
procedure).

For more on the Scott Knott procedure (where ANOVA is used to define
_useful_), see N. Mittas and L. Angelis, “Ranking and clustering
software cost estimation models through a multiple comparisons
algorithm,” IEEE Transactions on Software Engineering (pre-print),
2012.

|#

(defun mean (lst)
  (float (/ (reduce #'+ lst) 
	    (length lst))))

(defun flatten (thing)
  (cond ((null thing) nil)
        ((atom thing) `(,thing))
        (t (mapcan #'flatten thing))))

(defun percentiles (l &optional (collect '(0 25 50 75 100)))
  "one pass through a list of nums to 'collect' some percentiles"
  (let* (out
	 last
	 (pos -1)
	 (size (length l)))
    (dolist (one  l out)
      (incf pos)
      (if (null collect)
	  (return-from percentiles out))
      (let ((want (first collect))
	    (progress (* 100.0 (/ (1+ pos) size))))
	(if (>= progress want)
	    (push (cons (pop collect)
			(if (= progress want)
			    one
			    (* 0.5 (+ one (or last one)))))
		  out)))
      (setf last one))))

(defun nchars (n &optional (char #\Space))
  (with-output-to-string (s)
    (dotimes (i n)
      (format s "~a" char))))

(defstruct percentile label  breaks  positions key median)

(defun quintile (l &key (shrink 2))
  (let* ((width    100)
         (l1       (mapcar #'(lambda (x) (* x 100)) (sort (copy-list l) #'<)))
         (chops    (percentiles l1 '(0 10 30 50 70 90 100)))
         string
        )
    (labels ((squeeze0 (x) (round (/ x shrink)))
             (squeeze  (x) (min (- (squeeze0 width) 2) (squeeze0 x)))
             (string0  ()  (nchars (squeeze0 width)))
             (p        (n) (squeeze (cdr (assoc n chops))))
             (dot      (n char) (setf (char string n)  char))
             (paint    (n1 n2 char)
                          (setf (subseq string (p n1) (p n2))
                                (nchars (- (p n2) (p n1)) char))))
      (setf string (string0))
      ;(dot   (squeeze  0)   #\|)
      ;(dot   (squeeze 100)   #\|)
      ;(dot   (squeeze 50)   #\|)
      (paint 10 30 #\-)
      (paint 70 90 #\-)
      (dot  (p 50)  #\|)
      string)))

(defun _quintile0 (&optional (pow 2))
  (let (l1 (n 10000))
    (dotimes (i n)
      (push (expt  (/ (random 100) 100) pow) l1))
    (quintile l1)))

 #|
(deftest !quintile1 ()
   (test
    (!quintile0 0.5)
    "           --------     |   ---- "))

 (deftest !quintile2 ()
   (test
    (!quintile0 1)
    "   -------       |      ------   "))


(deftest !quintile3 ()
  (quintile '(1.0 1.0 0.969697 1.0 1.0 1.0 1.0
                  1.0 1.0 1.0 1.0 1.0 1.0 1.0
                  1.0 1.0 0.9714286 1.0 1.0 0.9714286 1.0)))

|#
(defstruct rx
  list
  name
  (more 0)
  (same 0)
  (rank 0)
  (prob t)
  mean)

(defun rx! (lst &optional (> #'>) name)
  (let ((out    (make-rx :name name))
	(sorted (sort lst >)))
    (setf (rx-list   out) sorted
	  (rx-mean out) (mean sorted))
    out))

(defun a12 (pop1 pop2 &optional (> #'>))
  (let ((m   (length pop1))
	(n   (length pop2))
	(one (rx! pop1 >))
	(two (rx! pop2 >)))
    (labels ((stop ()    (or (null (rx-list one))
			     (null (rx-list two))))
             (head  (x)   (car    (rx-list x)))
	     (size  (x)   (length (rx-list x)))
	     (next! (x)   (pop    (rx-list x)) x)
	     (more! (x n) (incf   (rx-more x)  n))
	     (same! (x)   (incf   (rx-same x)))
	     (walk (l1 l2)
	       (unless (stop)
		 (cond ((funcall > (head l1) (head l2))
			(more! l1 (size l2))
			(walk (next! l1) l2))
		       ((= (head l1) (head l2))
			(same! l1)
			(same! l2)
			(walk l1 (next! l2)))
		       (t 
			(walk l2 l1))))))
      (walk one two)
      (+ (/        (rx-more one) (* m n))
	 (/ (* 0.5 (rx-same one)) (* m n))))))

(defun ra12-split (treatments > &optional (big 0.71)) ; small=0.56, 0.64, 0.71
  (labels ((contents (treatments) 
	     (sort (flatten 
		    (mapcar #'rx-list treatments))
		   >))
	   (indifferent (lst1 lst2)
	     (let ((prob (a12 lst1 lst2)))
	       
	       (< prob big))))
    (let ((m (mean (contents treatments)))
	  (best -1)
	  cut)
      (dotimes (i (length treatments))
	(let* ((left   (subseq treatments 0 i))
	       (right  (subseq treatments i))
	       (lefts  (contents left))
	       (rights (contents right)))
	  (when (and lefts rights)
	    (let* ((leftm  (mean lefts))
		   (rightm (mean rights))
		   (leftn  (length lefts))
		   (rightn (length rights))
		   (tmp    (+ (* leftn   (expt (abs (- m leftm )) 2))
			      (* rightn  (expt (abs (- m rightm)) 2)))))
	      (when (and (> tmp best)
			 (not (indifferent lefts rights)))
		(setf cut i
		      best tmp))))))
      cut)))

(defun ra12-recurse (treatments >  &optional (rank 1))
  (let ((cut (ra12-split treatments >)))
    (if cut
	(let ((left  (subseq treatments 0 cut))
	      (right (subseq treatments cut)))
	  (setf rank (+ 1 (ra12-recurse left  > rank)))
	  (setf rank      (ra12-recurse right > rank)))
	(dolist (treatment treatments)
	  (setf (rx-rank treatment) rank)))
    rank))

		    
(defun ra12 (treatments &optional (> #'>))
  (let* ((out (sort 
	      (mapcar 
	       #'(lambda (lst) 
		   (rx! (cdr lst) > (car lst)))
	       treatments)
	      >
	      :key #'rx-mean))
	 (max-rank    
	  (ra12-recurse out >)))
    (dotimes (i max-rank)
      (let* ((j (1+ i))
	     (k (1+ j))
	     b4-items
	     after after-items)
	(dolist (one out)
	  (when (= (rx-rank one) j)
	    (dolist (item (rx-list one))
	      (push item b4-items)))
	  (when (= (rx-rank one) k)
	      (push one after)
	      (dolist (item (rx-list one))
		(push item after-items))))
	(if (and b4-items after-items)
	    (let ((p (a12 b4-items after-items >)))
	      (dolist (one after)
		(setf (rx-prob one) p))))))
    out))


;;;; misc


;;; demos

(defun _a120 (l1 l2)
  `(l1 ,l1 l2 ,l2 a12 ,(a12 l1 l2)))

(defun _a12a ()
  (_a120 '(10     6 5   3 2 1) 
         '(   8 7 6 5 4 3))
)
(defun _a12b ()
  (_a120 '(100 60 50 30 20 10           ) 
         '(                  8 7 6 5 4 3))
)
(defun _a12c ()
  (_a120 '(                  8 7 6 5 4 3)
	 '(100 60 50 30 20 10           )) 
)
(defun _a12d ()
  (_a120 '(10 9 8   7 6)
         '(     8 8   6 5 4))
)

(defun show-a (one)
  `(name ,(rx-name one)
	 mean ,(rx-mean one)
	 rank, (rx-rank one)
	 prob ,(rx-prob one)
	 list ,(rx-list one)))

(defun _ra12a ()
  (print (mapcar #'show-a
		 (ra12 '((x1 0.34 0.49 0.51 0.60)
			 (x2 0.6  0.7  0.8  0.90)
			 (x3 0.15 0.25 0.4  0.35)
			 (x4 0.6  0.7  0.8  0.90)
			 (x5 0.1  0.2  0.3  0.40))))))

(defun _ra12b ()
  (print (mapcar #'show-a
		 (ra12  '((y1 101 100 99   101  99.5)
			  (y2 101 100 99   101 100.0)
			  (y3 101 100 99.5 101  99.0)
			  (y4 101 100 99   101 100.0))))))

(defun _ra12c ()
  (let* ((n 25) ; 25 measurements
	 (maxs '(10 20 30 40 50 60 70 80 90  ; 36 treatments
		 110 120 130 140 150 160 170 180 190 
		 210 220 230 240 250 260 270 280 290 
		 310 320 330 340 350 360 370 380 390 
		 ))
	 (rxs
	  (mapcar #'(lambda (max)
		      (let (all)
			(dotimes (i n (cons (gensym) all))
			  (push (random max) all))))
		  maxs)))
    (time (mapcar #'show-a (ra12 rxs)))))

(defun tiles (l)
  (let* ((l1  (sort (flatten (mapcar #'cdr l)) #'<))
	 (min (car l1))
	 (max (car (last l1)))
	 (results (ra12 l)))
    (labels ((norm  (x)   (/ (- x min) (- max min)))
	     (norms (lst) (mapcar #'norm lst)))
        (format t "~&~a| ~4d% ~4d% ~4d% ~4d% ~4d% ~4d% ~4d% | rank" 
		(nchars 33) 0 10 30 50 70 90 100)
	(format t "~&~a|  ~a"
		(nchars 33) (nchars 48 #\-))
        (dolist (result results)
	  (let* ((tile  (quintile (norms (rx-list result)) :shrink 3))
		(breaks (percentiles (rx-list result)
				     '(0 10 30 50 70 90 100)))
		(rhs    (with-output-to-string (str)
		           (dolist (break breaks)
			     (format str " ~5,2f" (cdr break))))))
	    (format t "~&~a|~a |~2,d) ~a~%" 
		    tile rhs (rx-rank result) (rx-name result)))))))


(defun more (n noise &rest lst)
  (let (out)
    (dolist (one lst (reverse out))
      (dotimes (i n)
	(push (+ one (* (random noise) one))
	      out)))))

(defun _tiles()
  (let ((data  
	 (list
	  (cons 'x1 (more 10 0.1 0.34 0.49 0.51 0.60))
	  (cons 'x2 (more 10 0.1 0.6  0.7  0.8  0.90))
	  (cons 'x3 (more 10 0.1 0.0 0.25 0.4  0.35))
	  (cons 'x4 (more 10 0.1 0.6  0.7  0.8  0.90))
	  (cons 'x5 (more 10 0.1  0.2  0.3  0.40)))))
    (print data)
    (tiles data)))
