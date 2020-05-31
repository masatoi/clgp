(defpackage :clgp/tests/main
  (:use #:cl
        #:clgp/core/clgp
        #:rove))
(in-package :clgp/tests/main)

;; NOTE: To run this test file, execute `(asdf:test-system :clgp)' in your Lisp.

(deftest test-target-1
  (testing "should (= 1 1) to be true"
    (ok (= 1 1))))
