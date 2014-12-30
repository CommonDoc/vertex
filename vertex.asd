(defsystem vertex
  :author "Fernando Borretti <eudoxiahp@gmail.com>"
  :maintainer "Fernando Borretti <eudoxiahp@gmail.com>"
  :license "MIT"
  :version "0.1"
  :depends-on (:common-doc
               :plump-tex
               :anaphora)
  :components ((:module "src"
                :serial t
                :components
                ((:file "transform")
                 (:file "parser")
                 (:file "vertex"))))
  :description "A markup language with TeX syntax."
  :long-description
  #.(uiop:read-file-string
     (uiop:subpathname *load-pathname* "README.md"))
  :in-order-to ((test-op (test-op vertex-test))))
