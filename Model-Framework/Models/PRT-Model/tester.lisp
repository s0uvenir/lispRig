(load "main.lisp")

;;;;;;;;;;;;;;;;;;;;
;Test Cases Go Here;
;;;;;;;;;;;;;;;;;;;;

(format t "~%~%//Demo//~%~%")
(print-station beechurst)
(format t "//Adding People at Beechurst and Updating//~%~%")
(add-people beechurst engineering_goer 19)
(update  beechurst 500)
(add-people beechurst walnut_goer 4)
(update beechurst 300)
(print-station beechurst)
(print-station engineering)
(format t "//Sending car from Beechurst to Engineering//~%~%")
(send-car beechurst engineering)
(print-station beechurst)
(print-station engineering)
(format t "//Updating Beechurst and Engineering//~%~%")
(update beechurst 1000)
(print-station beechurst)
(update engineering 1000)
(print-station engineering)