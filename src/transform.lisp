(in-package :cl-user)
(defpackage vertex.transform
  (:use :cl)
  (:export :transform)
  (:documentation "Transform a plump-tex document into a CommonDoc document."))
(in-package :vertex.transform)

(defgeneric transform (obj)
  (:documentation "Transform a plump-tex node into a CommonDoc node."))

(defmethod transform ((node plump:text-node))
  (make-instance 'common-doc:<text-node>
                 :text (plump:text node)))

(defmethod transform ((vec vector))
  (make-instance 'common-doc:<content-node>
                 :children
                 (loop for elem across vec collecting
                   (transform elem))))

(defmethod transform ((root plump:root))
  (transform (plump:children root)))

(defmethod transform ((node plump:element))
  (let ((name (plump:tag-name node))
        (attr (plump:attributes node))
        (children (plump:children node)))
    nil))
