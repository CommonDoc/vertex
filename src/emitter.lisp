(in-package :cl-user)
(defpackage vertex.emitter
  (:use :cl)
  (:export :emit-to-stream)
  (:documentation "Create a VerTeX representation of a CommonDoc document."))
(in-package :vertex.emitter)

(defun emit-to-stream (document stream)
  (plump-tex:serialize (common-doc-plump.emitter:emit document)
                       stream))
