(defpackage #:clgp/core/utils
  (:use #:cl)
  (:import-from #:alexandria
                #:symbolicate)
  (:import-from #:uuid
                #:make-v4-uuid )
  (:export #:missing
           #:supplied-p
           #:define-typed-function
           #:random-string
           #:random-uuid))
(in-package #:clgp/core/utils)

(defmacro eval-when-always (&body body)
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     ,@body))

(defun missing (name &optional function-name)
  (error "Missing :~A in argument~@[of function ~A~]" name function-name))

(eval-when-always
  (defun gen-supplied-var (symbol)
    (symbolicate symbol '-supplied-p)))

(eval-when-always
  (defun split-docstring-and-rest-forms (body)
    (if (and (consp body) (stringp (first body)))
        (values (rest body)
                (first body))
        (values body nil))))

(defmacro define-typed-function (name
                                 (&rest default-parameters)
                                 (&rest key-parameters)
                                 &body body)
  (let ((key-parameters (loop :for param :in key-parameters
                              :collect (list (first param)
                                             (second param)
                                             (third param)
                                             (gen-supplied-var (first param)))))
        (default-parameters (loop :for param :in default-parameters
                                  :collect (list (first param)
                                                 (second param)))))
    (multiple-value-bind (body docstring)
        (split-docstring-and-rest-forms body)
      `(defun ,name (,@(mapcar #'first default-parameters)
                     &key ,@(loop :for (var type default supplied-var) :in key-parameters
                                  :collect `(,var ,default ,supplied-var)))
         ,@(when docstring (list docstring))
         ,@(loop :for (var type) :in default-parameters
                 :collect `(check-type ,var ,type))
         ,@(loop :for (var type default supplied-var) :in key-parameters
                 :collect `(when ,supplied-var
                             (check-type ,var ,type)))
         (macrolet ((supplied-p (name)
                      `(case ',name
                         ,@(loop :for (var type default supplied-var) :in ',key-parameters
                                 :collect `(,var ,supplied-var)))))
           ,@body)))))

(defun random-char ()
  (let ((*random-state* (make-random-state t)))
    (ecase (random 3)
      (0 (code-char (+ (char-code #\0) (random 10))))
      (1 (code-char (+ (char-code #\a) (random 26))))
      (2 (code-char (+ (char-code #\A) (random 26)))))))

(defun random-string (&optional (length 40))
  (let ((string (make-string length)))
    (dotimes (i length string)
      (setf (aref string i) (random-char)))))

(defun random-uuid ()
  (string-downcase (format nil "~A" (uuid:make-v4-uuid))))
