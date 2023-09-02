
(in-package :trivial-unit-tests)

(defclass mock-test ()
  ((name :reader test-name
         :initarg :name)
   (was-run :accessor was-run
            :initform nil))
  (:default-initargs :name (gensym)))

(defmethod run-test ((test mock-test))
  (setf (was-run test) t))

(setf (suite 'self-tests) (make-instance 'suite :name 'self-tests))

(setf (suite 'test-tests) (make-instance 'suite :name 'test-tests))

(setf (suite 'suite-tests) (make-instance 'suite :name 'suite-tests))

(deftest should-call-test-body (test-tests)
  (let* ((was-run nil)
         (test (make-instance 'test
                              :name 'example
                              :body (lambda () (setq was-run t)))))
    (assert (not was-run))
    (run-test test)
    (assert was-run)))

(deftest should-call-on-fail (test-tests)
  (let ((on-fail-called nil)
        (test (make-instance 'test
                             :name 'should-fail
                             :body (lambda () (error "Failing on purpose.")))))
    (defmethod on-fail ((test (eql test)) error)
      (setq on-fail-called t)
      (call-next-method))
    (assert (not on-fail-called))
    (with-open-stream (*standard-output* (make-broadcast-stream))
      (run-test test))
    (assert on-fail-called)))

(deftest should-print-failure-message (test-tests)
  (assert (string= (with-output-to-string (*standard-output*)
                     (run-test (make-instance
                                'test
                                :name 'should-fail
                                :body (lambda ()
                                        (error "Failing on purpose.")))))
                   "In SHOULD-FAIL: Failing on purpose.")))

(deftest should-call-all-tests-in-suite (suite-tests)
  (let ((suite (make-instance 'suite :name 'example))
        (tests (loop
                 for i from 1 to 3
                 collect (make-instance 'mock-test))))
    (dolist (test tests)
      (add-test suite test))
    (run-tests suite)
    (assert (every #'was-run tests))))

(deftest should-only-have-one-test-per-name (suite-tests)
  (let ((suite (make-instance 'suite :name 'example))
        (test1 (make-instance 'mock-test :name 'a-test))
        (test2 (make-instance 'mock-test :name 'a-test)))
    (add-test suite test1)
    (add-test suite test2)
    (run-tests suite)
    (assert (not (was-run test1)))
    (assert (was-run test2))))

(defparameter *self-test-suites*
  '(test-tests
    suite-tests))

(defun run-self-tests ()
  (dolist (suite-name *self-test-suites*)
    (run-tests (suite suite-name))))
