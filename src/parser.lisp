(in-package :cl-user)
(defpackage vertex.parser
  (:use :cl)
  (:export :parse-string
           :parse-file)
  (:documentation "Parse a TeX file into a CommonDoc document."))
(in-package :vertex.parser)

(defun parse-string (string)
  (vertex.transform:transform (plump-tex:parse string)))

(defun parse-file (pathname)
  (vertex.transform:transform (plump-tex:parse pathname)))
