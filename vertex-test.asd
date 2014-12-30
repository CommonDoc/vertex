(defsystem vertex-test
  :author "Fernando Borretti <eudoxiahp@gmail.com>"
  :license "MIT"
  :depends-on (:vertex
               :fiveam)
  :components ((:module "t"
                :serial t
                :components
                ((:file "parser")))))
