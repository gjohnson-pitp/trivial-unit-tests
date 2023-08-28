
(defpackage :trivial-unit-tests
  (:use :cl)
  (:export #:run-test
           #:on-fail
           #:test
           #:suite
           #:add-test
           #:run-tests
           #:deftest))
