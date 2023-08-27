
(in-package :trivial-unit-tests)

(defclass test ()
  ((name :reader test-name
         :initarg :name)
   (body :reader test-body
         :initarg :body)))

(defmethod run-test ((test test))
  (funcall (test-body test)))
