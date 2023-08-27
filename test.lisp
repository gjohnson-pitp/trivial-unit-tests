
(in-package :trivial-unit-tests)

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
                   "In SHOULD-FAIL: Failing on purpose.")))
