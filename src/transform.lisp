(in-package :cl-user)
(defpackage vertex.transform
  (:use :cl :anaphora)
  (:import-from :common-doc
                :<content-node>
           :<text-node>
               :<paragraph>
               :<markup>
               :<bold>
               :<italic>
               :<underline>
               :<strikethrough>
               :<code>
               :<superscript>
               :<subscript>
               :<code-block>
               :<verbatim>
               :<quote>
               :<inline-quote>
               :<block-quote>
               :<link>
               :<internal-link>
               :<external-link>
               :<web-link>
               :<list>
               :<list-item>
               :<definition>
               :<unordered-list>
               :<ordered-list>
               :<definition-list>
               :<image>
               :<figure>
               :<table>
               :<row>
               :<cell>
               :<section>
               :<document>)
  (:export :transform)
  (:documentation "Transform a plump-tex document into a CommonDoc document."))
(in-package :vertex.transform)

;;; Utilities

;;; Variables

(defparameter *transforms* (make-hash-table :test #'equal))

;;; Methods

(defgeneric transform (obj)
  (:documentation "Transform a plump-tex node into a CommonDoc node."))

(defmethod transform ((node plump:text-node))
  (make-instance '<text-node>
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

(defmacro define-trivial-transform (name class)
  `(define-transform ,name (children)
     (make-instance ',class
                    :children (transform children))))

;; Basic stuff

(define-trivial-transform "p" <paragraph>)

;; Markup

(define-trivial-transform "b" <bold>)
(define-trivial-transform "i" <italic>)
(define-trivial-transform "u" <underline>)
(define-trivial-transform "strike" <strikethrough>)
(define-trivial-transform "code" <code>)
(define-trivial-transform "sup" <superscript>)
(define-trivial-transform "sub" <subscript>)

;; Code

;; Quotes

(define-trivial-transform "q" <inline-quote>)
(define-trivial-transform "quote" <block-quote>)

;; Links

;; Lists

;; Figures

(define-attr-transform "image" (attributes children)
  (declare (ignore children))
  (make-instance '<image>
                 :source (gethash "source" attributes)
                 :description (gethash "desc" attributes)))

(define-transform "figure" (children)
  (let ((image
          (find-if #'(lambda (node)
                       (equal (plump:tag-name node) "image"))
                   children))
        (description
          (find-if-not #'(lambda (node)
                           (equal (plump:tag-name node) "image"))
                       children)))
    (make-instance '<figure>
                   :image image
                   :description
                   (make-instance '<content-node>
                                  :children (transform description)))))

;; Tables

;; Structure
