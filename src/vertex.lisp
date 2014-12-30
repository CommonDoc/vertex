(in-package :cl-user)
(defpackage vertex
  (:use :cl)
  (:export :<vertex>)
  (:documentation "The main interface."))
(in-package :vertex)

(defclass <vertex> (common-doc:<format>)
  ()
  (:documentation "The VerTeX format."))

(defmethod common-doc:parse-document ((vertex <vertex>)
                                      (string string))
  "Return a VerTeX document parsed from a string."
  (vertex.parser:parse-string string))

(defmethod common-doc:parse-document ((vertex <vertex>)
                                      (pathname pathname))
  "Return a VerTeX document parsed from a file."
  (vertex.parser:parse-file pathname))

(defmethod common-doc:emit-document ((vertex <vertex>)
                                     (doc common-doc:<document>)
                                     stream)
  (error "Not implemented."))
