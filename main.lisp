
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
test and the error object thrown. The default method on the predefined
test class outputs a string in the format \"In [test-name]: [error
message]\".

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
on-fail (function)
deftest (macro)"))

(defmethod run-test ((test test))
  (handler-case (funcall (test-body test))
    (error (error) (on-fail test error))))

(defmethod on-fail ((test test) error)
  (format t
          "~&In ~S: ~A"
          (test-name test)
          error))

(defgeneric add-test (suite test)
  (:documentation "(add-test suite test)

Adds the given test to the suite. For the method on the predefined
suite class, the test must have a test-name method, which should
return a symbol.

SEE ALSO:
suite (class)"))

(defgeneric run-tests (suite)
  (:documentation "(run-tests suite)

Runs all the tests in the given suite.

SEE ALSO:
suite (class)
test (class)
run-test (function)"))

(defclass suite ()
  ((name :reader suite-name
         :initarg :name)
   (test-table :reader suite-test-table
               :initform (make-hash-table)))
  (:documentation "(make-instance 'suite :name [a symbol])

A suite object that takes a symbol as a name. Add tests to it with
add-test, and run all the tests in a suite with run-tests. Save a
suite to refer to it by name later using (setf suite), and retrieve it
with (suite name).

SEE ALSO:
add-test (function)
run-tests (function)
test (class)
*suites* (variable)
suite (function)"))

(defmethod add-test ((suite suite) test)
  (setf (gethash (test-name test) (suite-test-table suite)) test))

(defmethod run-tests ((suite suite))
  (loop
    for test being the hash-values of (suite-test-table suite)
    do (run-test test)))

(defvar *suites*
  (make-hash-table)
  "Global table of test suites. Read it with (suite name), or
add/overwrite a suite with (setf (suite name) suite).

SEE ALSO:
suite (class, function)")

(defun suite (name)
  "(suite name)

Returns the suite with the given name. Suites can be registered
with (setf (suite name) suite).

SEE ALSO:
*suites* (variable)
suite (class)"
  (multiple-value-bind (suite foundp) (gethash name *suites*)
    (unless foundp
      (error "No suite named ~A" name))
    suite))

(defun (setf suite) (new-suite name)
  (setf (gethash name *suites*) new-suite))

(defmacro deftest (name (suite) &body body)
  "(deftest name (suite) [body])

Macro to define a new test. A test object is instantiated and added to
the suite named by the \"suite\" argument (not evaluated).

A test is considered to fail when an error is thrown from its body,
and to pass if no error is thrown.

SEE ALSO:
*suites* (variable)
suite (class, function)
test (class)"
  `(add-test (suite ',suite)
             (make-instance 'test :name ',name :body (lambda () ,@body))))
