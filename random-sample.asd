;;;; random-sample.asd

(asdf:defsystem #:random-sample
  :description "Random sample of a sequence with uniform distribution."
  :author "Paul M. Rodriguez <pmr@ruricolist.com>"
  :license "MIT"
  :in-order-to ((asdf:test-op (asdf:test-op "random-sample-tests")))
  :depends-on ("alexandria"
               "serapeum"
               "infix-math"
               "named-readtables")
  :serial t
  :components ((:file "package")
               (:file "readtable")
               (:file "random-sample")))

(asdf:defsystem :random-sample-tests
  :description "Tests for random-sample."
  :author "Paul M. Rodriguez <pmr@ruricolist.com>"
  :license "MIT"
  :perform (asdf:test-op (o c) (uiop:symbol-call :random-sample-tests :run-tests))
  :pathname "tests/"
  :depends-on ("fiveam" "random-sample")
  :components ((:file "random-sample-tests")))
