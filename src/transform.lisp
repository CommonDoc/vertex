(in-package :cl-user)
(defpackage vertex.transform
  (:use :cl :anaphora)
  (:export :transform)
  (:documentation "Transform a plump-tex document into a CommonDoc document."))
(in-package :vertex.transform)

;;; Variables

(defparameter *transforms* (make-hash-table :test #'equal))

;;; Methods

(defgeneric transform (obj)
  (:documentation "Transform a plump-tex node into a CommonDoc node."))

(defmethod transform ((node plump:text-node))
  (make-instance 'common-doc:<text-node>
                 :text (plump:text node)))

(defmethod transform ((vec vector))
  (loop for elem across vec collecting
    (transform elem)))

(defmethod transform ((root plump:root))
  (transform (plump:children root)))

(defmethod transform ((node plump:element))
  (let ((name (plump:tag-name node))
        (attributes (plump:attributes node))
        (children (plump:children node)))
    (aif (gethash name *transforms*)
         (funcall it attributes children)
         (error "No node with this name transform: ~A." name))))

;;; Transforms

(defmacro define-attr-transform (name (attrs args) &rest body)
  `(setf (gethash ,name *transforms*)
         #'(lambda (,attrs ,args)
             ,@body)))

(defmacro define-transform (name (args) &rest body)
  `(setf (gethash ,name *transforms*)
         #'(lambda (attrs ,args)
             (declare (ignore attrs))
             ,@body)))

;; Basic stuff

(define-transform "p" (children)
  (make-instance 'common-doc:<paragraph>
                 :children (transform children)))

;; Markup
