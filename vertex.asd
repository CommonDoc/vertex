(defsystem vertex
  :author "Fernando Borretti <eudoxiahp@gmail.com>"
  :maintainer "Fernando Borretti <eudoxiahp@gmail.com>"
  :license "MIT"
  :version "0.1"
  :depends-on (:common-doc
               :common-plump
               :plump-tex)
  :components ((:module "src"
                :serial t
                :components
                ((:file "parser")
                 (:file "vertex"))))
  :description "A markup language with TeX syntax."
  :long-description
  #.(uiop:read-file-string
     (uiop:subpathname *load-pathname* "README.md"))
  :in-order-to ((test-op (test-op vertex-test))))
