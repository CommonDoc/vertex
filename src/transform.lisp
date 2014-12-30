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

(defun find-tag-by-name (tag-name vector)
  (find-if #'(lambda (node)
               (if (plump:element-p node)
                   (equal (plump:tag-name node) tag-name)))
           vector))

(defun tags-without-name (tag-name vector)
  (find-if-not #'(lambda (node)
                   (if (plump:element-p node)
                       (equal (plump:tag-name node) tag-name)))
               vector))

(defun pop-by-name (tag-name vector)
  (delete-if #'(lambda (node)
                 (if (plump:element-p node)
                     (equal (plump:tag-name node) tag-name)))
             vector :count 1))

(defun serialize-to-string (node)
  (with-output-to-string (str)
    (plump-tex:serialize node str)))

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
  (make-instance '<document>
                 :children (transform (plump:children root))))

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

(define-attr-transform "codeblock" (attributes children)
  (let ((language (gethash "lang" attributes)))
    (unless language
      (error "Need to specify a language tag."))
    (make-instance '<code-block>
                   :language language
                   :children (transform children))))

(define-transform "verb" (children)
  (make-instance '<verbatim>
                 :text (serialize-to-string children)))

;; Quotes

(define-trivial-transform "q" <inline-quote>)
(define-trivial-transform "quote" <block-quote>)

;; Links

;; Lists

(define-transform "list" (items)
  (make-instance '<unordered-list> :items (transform items)))

(define-transform "enum" (items)
  (make-instance '<ordered-list> :items (transform items)))

(define-transform "deflist" (items)
  (make-instance '<definition-list> :items (transform items)))

(define-trivial-transform "item" <list-item>)

(define-transform "def" (children)
  (let ((term (find-tag-by-name "term" children))
        (definition (tags-without-name "term" children)))
    (make-instance '<definition>
                   :term (transform term)
                   :definition (make-instance '<content-node>
                                              :children (transform definition)))))

;; Figures

(define-attr-transform "image" (attributes children)
  (declare (ignore children))
  (make-instance '<image>
                 :source (gethash "source" attributes)
                 :description (gethash "desc" attributes)))

(define-transform "figure" (children)
  (let ((image (find-tag-by-name "image" children))
        (description (tags-without-name "image" children)))
    (make-instance '<figure>
                   :image image
                   :description
                   (make-instance '<content-node>
                                  :children (transform description)))))

;; Tables

(define-transform "cell" (children)
  (make-instance '<cell> :children (transform children)))

(define-transform "row" (cells)
  (make-instance '<row> :children (transform cells)))

(define-transform "table" (rows)
  (make-instance '<table> :rows (transform rows)))

;; Structure

(define-attr-transform "section" (attributes children)
  (let ((title (aif (gethash "title" attributes)
                    ;; We got the title from the attributes
                    (make-instance '<text-node>
                                   :text it)
                    ;; Otherwise, look for a title tag in the children
                    (aif (find-tag-by-name "title" children)
                         (transform it)
                         (error "Untitled section."))))
        (children (tags-without-name "title" children))
        (reference (gethash "ref" attributes)))
    (make-instance '<section> :title title :reference reference :children children)))
