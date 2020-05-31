(defpackage #:clgp/core/clgp
  (:use #:cl
        #:iterate)
  (:import-from #:clgp/core/utils
                #:define-typed-function
                #:random-uuid)
  (:import-from #:alexandria
                #:if-let
                #:when-let)
  (:nicknames #:clgp)
  (:export #:plot
           #:graph
           #:display))
(in-package #:clgp/core/clgp)

(define-condition gnuplot-error (error) ())

(defparameter *gnuplot-command* "gnuplot")
(defparameter *tmp-dir* #P"/tmp/")

(defun run (gp-file)
  (check-type gp-file (or string pathname))
  (let ((file-path-str (format nil "~A" gp-file)))
    (uiop:run-program `(,*gnuplot-command* "-persist" ,file-path-str))))

(defgeneric dump-gp (stream object))
(defgeneric dump-dat (stream object))
(defgeneric delete-dat-file (object))

(defun delete-all-dat-files ()
  (let ((dat-files
          (remove-if-not (lambda (p)
                           (and (= (length (pathname-name p)) 17)
                                (equal (subseq (pathname-name p) 0 5) "clgp-")
                                (equal (pathname-type p) "dat")))
                         (uiop:directory-files *tmp-dir*))))
    (dolist (f dat-files)
      (uiop:delete-file-if-exists f))))

;;; Classes

(defclass graph ()
  ((id :initform (random-uuid)
       :accessor graph-id)
   (plots :initarg :plots
          :initform nil
          :type (or list vector)
          :accessor graph-plots)
   (legend :initarg :legend
           :initform nil
           :accessor graph-legend)
   (title :initarg :title
          :initform nil
          :accessor graph-title)
   (x-label :initarg :x-label
          :initform nil
          :accessor graph-x-label)
   (y-label :initarg :y-label
          :initform nil
          :accessor graph-y-label)
   (x-range :initarg :x-range
          :initform nil
          :accessor graph-x-range)
   (y-range :initarg :y-range
          :initform nil
          :accessor graph-y-range)
   (aspect-ratio :initarg :aspect-ratio
                 :initform nil
                 :accessor graph-aspect-ratio)
   (gp-file :initarg :gp-file
            :initform nil
            :accessor graph-gp-file)))

(defclass plot ()
  ((id :initform (random-uuid)
       :accessor plot-id)
   (style :initarg :style
          :initform :lines
          :type (member :lines :points :impulses)
          :accessor plot-style)
   (title :initarg :title
          :initform ""
          :type string
          :accessor plot-title)
   (x :initform nil
      :initarg :x
      :type (or list vector)
      :accessor plot-x)
   (y :initform nil
      :initarg :y
      :type (or list vector)
      :accessor plot-y)
   (data-file :initform nil
              :type (or null pathname)
              :accessor plot-data-file)
   (axis :initarg :axis
         :initform :x1y1
         :type (member :x1y1 :x2y2)
         :accessor plot-axis)))

(define-typed-function plot ((y (or list vector)))
    ((x (or list vector null))
     (style (member :lines :points :impulses) :lines)
     (title string "")
     (axis (member :x1y1 :x2y2) :x1y1))
  "Constructor of plot"
  (assert (every #'numberp y))
  (when x (assert (every #'numberp x)))
  (make-instance 'plot :y y :x x :style style :title title :axis axis))

(defmethod delete-dat-file ((plot plot))
  (uiop:delete-file-if-exists (plot-data-file plot)))

(defmethod delete-dat-file ((graph graph))
  (dolist (plot (graph-plots graph))
    (delete-dat-file plot)))

(defmethod initialize-instance ((plot plot) &rest initargs)
  (declare (ignore initargs))
  (call-next-method)
  (let ((file-path (merge-pathnames (pathname (format nil "clgp-~A.dat" (plot-id plot)))
                                    *tmp-dir*)))
    (with-open-file (file file-path
                        :direction :output
                        :if-exists :supersede)
      (dump-dat file plot))
    (setf (plot-data-file plot) file-path)
    plot))

(defun comma-separated-concatenate (string-list)
  (reduce (lambda (s1 s2) (concatenate 'string s1 "," s2))
          string-list))

(defmethod dump-gp ((stream stream) (graph graph))
  ;; Main title
  (when (graph-title graph)
    (format stream "set title \"~A\"~%" (graph-title graph)))
  ;; Axis label
  (when-let ((x-label (graph-x-label graph)))
    (format stream "set xlabel \"~A\"~%" x-label))
  (when-let ((y-label (graph-y-label graph)))
    (format stream "set ylabel \"~A\"~%" y-label))

  ;; Input range, Increase direction of X
  (if-let ((x-range (graph-x-range graph)))
    (format stream "set xrange [~f:~f] " (car x-range) (cadr x-range))
    (format stream "set xrange [] "))
  ;; (if x-range-reverse
  ;;     (format stream "reverse"))
  (format stream "~%")
  
  (if-let ((y-range (graph-y-range graph)))
    (format stream "set yrange [~f:~f] " (car y-range) (cadr y-range))
    (format stream "set yrange [] "))
  ;; (if y-range-reverse
  ;;     (format stream "reverse"))
  (format stream "~%")

  ;; ;; Use of logscale
  ;; (if x-logscale (format stream "set logscale x~%"))
  ;; (if y-logscale (format stream "set logscale y~%"))

  ;; Aspect ratio
  (when-let ((aspect-ratio (graph-aspect-ratio graph)))
    (format stream "set size ratio ~f~%" aspect-ratio))

  ;; ;; Graph legend enable/disable, or its position
  ;; key
  (format stream "plot ~A~%"
          (comma-separated-concatenate
           (iter (for plot in-sequence (graph-plots graph))
             (collect (format nil "\"~A\" using 1:2 with ~A title \"~A\" axis ~A"
                              (plot-data-file plot)
                              (string-downcase (plot-style plot))
                              (plot-title plot)
                              (string-downcase (plot-axis plot))))))))

(defmethod dump-dat ((stream stream) (plot plot))
  (let ((x-seq (plot-x plot))
        (y-seq (plot-y plot)))
    (if x-seq
        (iter (for x in-sequence x-seq)
              (for y in-sequence y-seq)
              (format stream "~f ~f~%" x y))
        (iter (for x from 0 by 1)
              (for y in-sequence y-seq)
              (format stream "~f ~f~%" x y)))))

(defun graph (plots &key title x-label y-label x-range y-range aspect-ratio)
  (let* ((graph (make-instance 'graph :plots plots
                                      :title title
                                      :x-label x-label
                                      :y-label y-label
                                      :x-range x-range
                                      :y-range y-range
                                      :aspect-ratio aspect-ratio))
         (file-path (merge-pathnames (pathname (format nil "clgp-~A.gp" (graph-id graph)))
                                     *tmp-dir*)))
    (setf (graph-gp-file graph) file-path)
    (with-open-file (file file-path
                          :direction :output
                          :if-exists :supersede)
      (dump-gp file graph))
    graph))

(defmethod display ((graph graph))
  (run (graph-gp-file graph)))
