
(in-package :trivial-unit-tests)

(defclass mock-test ()
  ((name :reader test-name
         :initform (gensym))
   (was-run :accessor was-run
            :initform nil)))

(defmethod run-test ((test mock-test))
  (setf (was-run test) t))

(defun run-self-tests ()
  ;; Should call test body
  (let* ((was-run nil)
         (test (make-instance 'test
                              :name 'example
                              :body (lambda () (setq was-run t)))))
    (assert (not was-run))
    (run-test test)
    (assert was-run))
  ;; Should call on-fail if the test body throws an error
  (let ((on-fail-called nil)
        (test (make-instance 'test
                             :name 'should-fail
                             :body (lambda () (error "Failing on purpose.")))))
    (defmethod on-fail ((test (eql test)) error)
      (setq on-fail-called t)
      (call-next-method))
    (assert (not on-fail-called))
    (run-test test)
    (assert on-fail-called))
  ;; on-fail should print a failure message to standard output
  (assert (string= (with-output-to-string (*standard-output*)
                     (run-test (make-instance 'test
                                              :name 'should-fail
                                              :body (lambda () (error "Failing on purpose.")))))
                   "In SHOULD-FAIL: Failing on purpose."))
  ;; Test suites should call all tests added to them
  (let ((suite (make-instance 'suite :name 'example))
        (tests (loop
                 for i from 1 to 3
                 collect (make-instance 'mock-test))))
    (dolist (test tests)
      (add-test suite test))
    (run-tests suite)
    (assert (every #'was-run tests))))
