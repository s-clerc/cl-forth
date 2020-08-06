(defsystem "cl-forth"
  :version "0.0.0"
  :author "Sam de Clerc"
  :license "GPLv3"
  :depends-on ("split-sequence" "cl-utilities")
  :components ((:module "src"
                :components
                ((:file "main")
                 (:file "environment")
                 (:file "words")
                 (:file "run"))))
  :description ""
  :in-order-to ((test-op (test-op "cl-forth/tests"))))

(defsystem "cl-forth/tests"
  :author "Sam de Clerc"
  :license "GPLv3"
  :depends-on ("cl-forth"
               "rove")
  :components ((:module "tests"
                :components
                ((:file "main"))))
  :description "Test system for cl-forth"
  :perform (test-op (op c) (symbol-call :rove :run c)))
