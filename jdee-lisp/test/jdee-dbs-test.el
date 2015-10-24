;;; package --- Summary
;;; Commentary:
;;; Code:

(require 'ert)
(require 'jdee-dbs)

(ert-deftest test-jdee-dbs-proc ()
  "Test creation of a jdee-dbs-proc instance"
  (should (jdee-dbs-proc (format "process%d" 100)
                        :id 100
                        :main-class "jmath.Test")))

(provide 'jdee-dbs-test)
;;; jdee-dbs-test.el ends here
