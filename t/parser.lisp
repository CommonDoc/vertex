(in-package :cl-user)
(defpackage vertex-test.parser
  (:use :cl :fiveam :common-doc)
  (:import-from :vertex.parser
                :parse-string))
(in-package :vertex-test.parser)

;;; Utilities

(defun first-child (document)
  (first (children document)))

(defmacro with-first-doc ((string doc) &rest body)
  `(let ((,doc (first-child (parse-string ,string))))
     ,@body))

(defmacro trivial-check (tag class)
  `(with-first-doc (,(format nil "\\~A{test}" tag) doc)
    (is-true
     (typep doc ',class))
    (is
     (equal (text (first-child doc)) "test"))))

(defmacro verbatim-check (text)
  `(with-first-doc (,(format nil "\\verb{~A}" text) doc)
     (is-true (typep doc '<verbatim>))
     (is (equal (text doc) ,text))))

;;; Tests

(def-suite tests
  :description "vertex tests.")
(in-suite tests)

(test text
  (with-first-doc ("test" doc)
    (is-true
     (typep doc '<text-node>))
    (is (equal (text doc) "test"))))

(test paragraph
  (trivial-check "p" <paragraph>))

(test markup
  (trivial-check "b" <bold>)
  (trivial-check "i" <italic>)
  (trivial-check "u" <underline>)
  (trivial-check "strike" <strikethrough>)
  (trivial-check "code" <code>)
  (trivial-check "sup" <superscript>)
  (trivial-check "sub" <subscript>))

(test quotes
  (trivial-check "q" <inline-quote>)
  (trivial-check "quote" <block-quote>))

(test code-block
  (with-first-doc ("\\codeblock[lang=lisp]{test}" doc)
    (is-true (typep doc '<code-block>))
    (is (equal (language doc) "lisp"))))

(test verbatim
  (verbatim-check "test")
  (verbatim-check "a \\b{2} b")
  (verbatim-check "\\a{\\b{\\c{test}}}"))

(run! 'tests)
