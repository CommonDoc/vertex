(in-package :cl-user)
(defpackage vertex.parser
  (:use :cl)
  (:export :parse-string
           :parse-file)
  (:documentation "Parse a TeX file into a CommonDoc document."))
(in-package :vertex.parser)

(defun input->common-doc (input)
  (let ((common-doc-plump.parser:*serializer* #'plump-tex:serialize))
    (common-doc-plump.parser:parse-document (plump-tex:parse input))))

(defun parse-string (string)
  "Parse a VerTeX string."
  (input->common-doc string))

(defun parse-file (pathname)
  "Parse a VerTeX file."
  (input->common-doc pathname))
