
(in-package :trivial-unit-tests)

(defgeneric run-test (test)
  (:documentation "(run-test test)

Runs the given test by calling the closure given as its body. The
default method on the predefined test class catches any error thrown
in the test's body and calls on-fail, passing it the test and the
error.

SEE ALSO:
test (class)
on-fail (function)"))

(defgeneric on-fail (test error)
  (:documentation "(on-fail test error)

Hook called when a test fails. By default, called in run-test when the
test body throws an error. It's passed the test object for the failed
test and the error object thrown.

SEE ALSO:
run-test (function)
test (class)"))

(defclass test ()
  ((name :reader test-name
         :initarg :name)
   (body :reader test-body
         :initarg :body))
  (:documentation "(make-instance 'test :name [a symbol] :body [a closure])

A test object that takes a symbol as a name and a closure as a
body. Run it with run-test.

SEE ALSO:
run-test (function)
on-fail (function)"))

(defmethod run-test ((test test))
  (handler-case (funcall (test-body test))
    (error (error) (on-fail test error))))

(defmethod on-fail ((test test) error)
  (format t
          "~&In ~S: ~A"
          (test-name test)
          error))
