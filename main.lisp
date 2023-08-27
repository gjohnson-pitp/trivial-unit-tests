
(in-package :trivial-unit-tests)

(defgeneric run-test (test)
  (:documentation "(run-test test)

Runs the given test by calling the closure given as its body.

SEE ALSO:
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
run-test (function)"))

(defmethod run-test ((test test))
  (handler-case (funcall (test-body test))
    (error (error) (on-fail test error))))

(defmethod on-fail ((test test) error))
