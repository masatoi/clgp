(in-package :clgp)

(defparameter x (loop for x from (- pi) to pi by 0.1 collect x))
(defparameter y1 (mapcar #'sin x))
(defparameter y2 (mapcar #'cos x))
(defparameter y3 (mapcar #'tan x))
(defparameter y4 (mapcar #'exp x))

(plot y1)

(plot (list y1 y2))

(plot (line y1))

(plot (list (line y1)
            (line y2)))

(plot (list (impulses y1)
            (points y2)))

(plot (graph (list (impulses y1)
                   (points y2))
             :aspect-ratio 1))

(plot (list (line y1 :x x)
            (line y4 :x x)))

(plot (list (line y1 :x x :axis :x1y1)
            (line y4 :x x :axis :x2y2)))

(plot (graph (list (line y1 :x x :title "sin")
                   (line y2 :x x :title "cos")
                   (line y3 :x x :title "tan"))
             :title "Trigonometric functions"
             :x-label "x"
             :y-label "y"
             :x-range (list (- pi) pi)
             :y-range '(-1 1)
             :aspect-ratio 1))
