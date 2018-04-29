;;; jdee-it-test.el --- JDEE integration tests -*- lexical-binding: t; -*-

;;; Commentary:
;; This are integration tests for JDEE.

;;; Code:

(require 'ert)

(ert-deftest jdee-should-load-all-its-modules ()
  (require 'jdee)
  (should (featurep 'jdee-refactor)))

(provide 'jdee-it-test)
;;; jdee-it-test.el ends here
