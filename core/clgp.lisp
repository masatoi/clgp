(defpackage #:clgp/core/clgp
  (:use #:cl
        #:iterate)
  (:import-from #:clgp/core/utils
                #:define-typed-function
                #:random-string)
  (:import-from #:alexandria
                #:if-let
                #:when-let)
  (:nicknames #:clgp)
  (:export #:plot
           #:graph))
(in-package #:clgp/core/clgp)

;;; Variables

(defparameter *gnuplot-command* "gnuplot")
(defparameter *tmp-dir* #P"/tmp/")


;;; 

(define-condition gnuplot-error (error) ())

(defun working-dir ()
  (let ((working-dir-path (merge-pathnames "clgp/" (uiop:ensure-directory-pathname *tmp-dir*))))
    (uiop:ensure-all-directories-exist (list working-dir-path))
    working-dir-path))

(defun run (gp-file)
  (check-type gp-file (or string pathname))
  (let ((file-path-str (format nil "~A" gp-file)))
    (uiop:run-program `(,*gnuplot-command* "-persist" ,file-path-str))))

(defgeneric dump-gp (stream object))
(defgeneric dump-dat (stream object))
(defgeneric delete-dat-file (object))

(defun delete-all-files ()
  (let ((dat-files
          (remove-if-not (lambda (p)
                           (and (equal (subseq (pathname-name p) 0 5) "clgp-")
                                (or (equal (pathname-type p) "dat")
                                    (equal (pathname-type p) "gp"))))
                         (uiop:directory-files (working-dir)))))
    (dolist (f dat-files)
      (uiop:delete-file-if-exists f))))

;;; Classes

(defclass graph ()
  ((id :initform (random-string)
       :accessor graph-id)
   (seqs :initarg :seqs
          :initform nil
          :type sequence
          :accessor graph-seqs)
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

(defclass seq ()
  ((id :initform (random-string)
       :accessor seq-id)
   (style :initarg :style
          :initform :lines
          :type (member :lines :points :impulses)
          :accessor seq-style)
   (title :initarg :title
          :initform ""
          :type string
          :accessor seq-title)
   (x :initform nil
      :initarg :x
      :type sequence
      :accessor seq-x)
   (y :initform nil
      :initarg :y
      :type sequence
      :accessor seq-y)
   (data-file :initform nil
              :type (or null pathname)
              :accessor seq-data-file)
   (axis :initarg :axis
         :initform :x1y1
         :type (member :x1y1 :x2y2)
         :accessor seq-axis)))

(defun seqp (object)
  (typep object 'seq))

(defmethod initialize-instance ((seq seq) &rest initargs)
  (declare (ignore initargs))
  (call-next-method)
  (let ((file-path (merge-pathnames (pathname (format nil "clgp-~A.dat" (seq-id seq)))
                                    (working-dir))))
    (with-open-file (file file-path
                        :direction :output
                        :if-exists :supersede)
      (dump-dat file seq))
    (setf (seq-data-file seq) file-path)
    seq))

(define-typed-function seq ((y sequence))
    ((x (or sequence null))
     (style (member :lines :points :impulses) :lines)
     (title string "")
     (axis (member :x1y1 :x2y2) :x1y1))
  "Constructor of seq"
  (assert (every #'numberp y))
  (when x (assert (every #'numberp x)))
  (make-instance 'seq :y y :x x :style style :title title :axis axis))

(defclass line (seq) ())

(define-typed-function line ((y sequence))
    ((x (or sequence null))
     (title string "")
     (axis (member :x1y1 :x2y2) :x1y1))
  "Constructor of line"
  (make-instance 'line :y y :x x :style :lines :title title :axis axis))

(defclass points (seq) ())

(define-typed-function points ((y sequence))
    ((x (or sequence null))
     (title string "")
     (axis (member :x1y1 :x2y2) :x1y1))
  "Constructor of points"
  (make-instance 'points :y y :x x :style :points :title title :axis axis))

(defclass impulses (seq) ())

(define-typed-function impulses ((y sequence))
    ((x (or sequence null))
     (title string "")
     (axis (member :x1y1 :x2y2) :x1y1))
  "Constructor of impulses"
  (make-instance 'impulses :y y :x x :style :impulses :title title :axis axis))

;;; delete temp files

(defmethod delete-dat-file ((seq seq))
  (uiop:delete-file-if-exists (seq-data-file seq)))

(defmethod delete-dat-file ((graph graph))
  (dolist (seq (graph-seqs graph))
    (delete-dat-file seq)))

(defmethod delete-gp-file ((graph graph))
  (uiop:delete-file-if-exists (graph-gp-file graph)))

;;; dump gp file

(defun comma-separated-concatenate (string-list)
  (reduce (lambda (s1 s2) (concatenate 'string s1 "," s2))
          string-list))

(defmethod dump-gp ((stream stream) (graph graph))

  #+(or unix linux)
  (format stream "set terminal x11~%")

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
           (iter (for seq in-sequence (graph-seqs graph))
             (collect (format nil "\"~A\" using 1:2 with ~A title \"~A\" axis ~A"
                              (seq-data-file seq)
                              (string-downcase (seq-style seq))
                              (seq-title seq)
                              (string-downcase (seq-axis seq))))))))

(defmethod dump-dat ((stream stream) (seq seq))
  (let ((x-seq (seq-x seq))
        (y-seq (seq-y seq)))
    (if x-seq
        (iter (for x in-sequence x-seq)
              (for y in-sequence y-seq)
              (format stream "~f ~f~%" x y))
        (iter (for x from 0 by 1)
              (for y in-sequence y-seq)
              (format stream "~f ~f~%" x y)))))

(defun graph (seq-or-seqs &key title x-label y-label x-range y-range aspect-ratio)
  (check-type seq-or-seqs (or seq sequence))
  (when (typep seq-or-seqs 'sequence)
    (assert (every #'seqp seq-or-seqs)))
  (let* ((graph (make-instance 'graph :seqs (if (typep seq-or-seqs 'sequence) seq-or-seqs (list seq-or-seqs))
                                      :title title
                                      :x-label x-label
                                      :y-label y-label
                                      :x-range x-range
                                      :y-range y-range
                                      :aspect-ratio aspect-ratio))
         (file-path (merge-pathnames (pathname (format nil "clgp-~A.gp" (graph-id graph)))
                                     (working-dir))))
    (setf (graph-gp-file graph) file-path)
    (with-open-file (file file-path
                          :direction :output
                          :if-exists :supersede)
      (dump-gp file graph))
    graph))

;;; plot methods

(defgeneric plot (object)
  (:documentation "plot data

Examples:

;; Plot single line from sequence of numbers
(plot '(1 2 3))

;; Multiple plot
(plot '((1 2 3)
        (3 2 1)))
"))

(defmethod plot ((graph graph))
  (run (graph-gp-file graph)))

(defmethod plot ((seq seq))
  (let ((g (graph seq)))
    (plot g)))

(defmethod plot ((s sequence))
  (let ((g (cond
             ;; sequence of numbers
             ((every #'numberp s)
              (graph (line s)))
             ;; sequence of sequence of numbers
             ((every (lambda (in-s)
                       (and (typep in-s 'sequence)
                            (every #'numberp in-s)))
                     s)
              (graph (iter (for in-s in-sequence s)
                       (collect (line in-s)))))
             ;; sequence of seq object
             ((every #'seqp s) (graph s))
             ;; others
             (t (error "Not a sequence of numbers, nor sequence of sequence.")))))
    (plot g)))
