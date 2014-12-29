(in-package :cl-user)
(defpackage vertex.parser
  (:use :cl)
  (:export :parse-string
           :parse-file)
  (:documentation "Parse a TeX file into a plump-tex document."))
(in-package :vertex.parser)

(defun parse-string (string)
  (plump-tex:parse string))

(defun parse-file (pathname)
  (plump-tex:parse pathname))
