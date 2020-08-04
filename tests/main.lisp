(defpackage cl-forth/tests/main
  (:use :cl
        :cl-forth
        :rove))
(in-package :cl-forth/tests/main)

;; NOTE: To run this test file, execute `(asdf:test-system :cl-forth)' in your Lisp.

(deftest test-target-1
  (testing "should (= 1 1) to be true"
    (ok (= 1 1))))
