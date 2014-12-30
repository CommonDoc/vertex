(in-package :cl-user)
(defpackage vertex
  (:use :cl :common-doc.format)
  (:export :<vertex>)
  (:documentation "The main interface."))
(in-package :vertex)

(defclass <vertex> (<format>)
  ()
  (:documentation "The VerTeX format."))

(defmethod parse-document ((vertex <vertex>)
                           (string string))
  "Return a VerTeX document parsed from a string."
  (vertex.parser:parse-string string))

(defmethod parse-document ((vertex <vertex>)
                           (pathname pathname))
  "Return a VerTeX document parsed from a file."
  (vertex.parser:parse-file pathname))

(defmethod emit-document ((vertex <vertex>)
                          (doc common-doc:<document>)
                          stream)
  (error "Not implemented."))
