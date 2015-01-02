(in-package :cl-user)
(defpackage vertex.parser
  (:use :cl)
  (:export :parse-string
           :parse-file)
  (:documentation "Parse a TeX file into a CommonDoc document."))
(in-package :vertex.parser)

(defun input->common-doc (input)
  (common-doc-plump.parser:parse (plump-tex:parse input)))

(defun parse-string (string)
  (input->common-doc string))

(defun parse-file (pathname)
  (input->common-doc pathname))
