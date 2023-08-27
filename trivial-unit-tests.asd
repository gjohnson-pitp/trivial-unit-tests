
(defsystem "trivial-unit-tests"
  :description "A minimal bootstrapped unit testing library."
  :author "Gabriel Johnson <gabriel.johnson.pitp@gmail.com>"
  :license "MIT"
  :serial t
  :components ((:file "package")
               (:file "main")
               (:file "test")))
