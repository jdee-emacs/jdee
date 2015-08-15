;;; package --- Summary
;;; Commentary:
;;; Code:

(require 'ert)
(require 'jde-dbs)

(ert-deftest test-jde-dbs-proc ()
  "Test creation of a jde-dbs-proc instance"
  (should (jde-dbs-proc (format "process%d" 100)
                        :id 100
                        :main-class "jmath.Test")))

(provide 'jde-dbs-test)
;;; jde-dbs-test.el ends here
