#-asdf3.1 (error "clgp requires ASDF 3.1 or later.")
(defsystem "clgp"
  :version "0.1.0"
  :author "Satoshi Imai"
  :license "MIT"
  :class :package-inferred-system
  :depends-on (:alexandria
               :uuid
               :iterate
               :clgp/core/clgp)
  :description ""
  :in-order-to ((test-op (test-op "clgp/tests"))))

(defsystem "clgp/tests"
  :author "Satoshi Imai"
  :license "MIT"
  :class :package-inferred-system
  :depends-on (:clgp/core/clgp
               "rove"
               :clgp/tests/main)
  :description "Test system for clgp"
  :perform (test-op (op c) (symbol-call :rove :run c)))
