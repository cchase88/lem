(defpackage :lem/html-renderer
  (:use :cl :lem))
(in-package :lem/html-renderer)

(defmethod print-object ((object paragraph) stream)
  (print-unreadable-object (object stream :type t)
    (with-slots (elements) object
      (prin1 elements stream))))

(defclass h1 ()
  ((text :initarg :text)))

(defmethod print-object ((object h1) stream)
  (print-unreadable-object (object stream :type t)
    (with-slots (text) object
      (prin1 text stream))))

(defvar *elements-so-far* '())

(defun call-with-collector (function)
  (let ((*elements-so-far* '()))
    (funcall function)
    (nreverse *elements-so-far*)))

(defmacro with-collector (() &body body)
  `(call-with-collector (lambda () ,@body)))

(defun collect (element)
  (push element *elements-so-far*))

(defgeneric traverse-aux (tag body))

(defmethod traverse-aux ((tag (eql :p)) body)
  (call-next-method))

(defmethod traverse-aux ((tag (eql :h1)) body)
  (collect (make-instance 'h1 :text (second body))))

(defmethod traverse-aux (tag body)
  (map () #'traverse (rest body)))

(defun traverse (lhtml)
  (etypecase lhtml
    (string
     (collect lhtml))
    (cons
     (traverse-aux (first lhtml) (rest lhtml)))))
  
(defun parse-html (string)
  (let ((lhtml (chtml:parse string (chtml:make-lhtml-builder))))
    (micros:micros-print lhtml)
    (with-collector () (traverse lhtml))))

(defun parse-markdown (string)
  (parse-html
   (with-output-to-string (out)
     (3bmd:parse-string-and-print-to-stream string out))))
