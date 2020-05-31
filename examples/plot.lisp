(in-package :clgp)

(defparameter x (loop for x from (- pi) to pi by 0.1 collect x))
(defparameter y1 (mapcar #'sin x))
(defparameter y2 (mapcar #'cos x))
(defparameter y3 (mapcar #'tan x))

(display
 (graph (list (plot y1 :x x :title "sin")
              (plot y2 :x x :title "cos")
              (plot y3 :x x :title "tan"))
        :title "Trigonometric functions"
        :x-label "x"
        :y-label "y"
        :x-range (list (- pi) pi)
        :y-range '(-1 1)
        :aspect-ratio 1))
