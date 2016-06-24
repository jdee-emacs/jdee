;;; package --- Unit tests for class parsing
;;; Commentary:
;;; Code:

(require 'ert)

(require 'jdee-parse)

(ert-deftest jdee--parse-integer-p-test ()
  (should (jdee--parse-integer-p "42"))
  (should (jdee--parse-integer-p "-42"))
  (should (jdee--parse-integer-p "4_2"))
  (should (jdee--parse-integer-p "-4_2")))

(ert-deftest jdee--parse-long-p-test ()
  (should (jdee--parse-long-p "42L"))
  (should (jdee--parse-long-p "42l"))
  (should (jdee--parse-long-p "-42l"))
  (should (jdee--parse-long-p "1_234l"))
  (should (jdee--parse-long-p "-4_56_3L")))

(ert-deftest jdee--parse-float-p-test ()
  (should (jdee--parse-float-p "3.14f"))
  (should (jdee--parse-float-p "-3.14f"))
  (should (jdee--parse-float-p "314f"))
  (should (jdee--parse-float-p "-314f"))
  (should (jdee--parse-float-p "3.14F"))
  (should (jdee--parse-float-p "-3.14F"))
  (should (jdee--parse-float-p "314F"))
  (should (jdee--parse-float-p "-314F"))
  (should (jdee--parse-float-p "-3.14_15F"))
  (should (jdee--parse-float-p "-314_555.7F"))
  (should (jdee--parse-float-p "-314_555.7_8F")))

(ert-deftest jdee--parse-double-p-test ()
  (should (jdee--parse-double-p "3.14"))
  (should (jdee--parse-double-p "-3.14"))
  (should (jdee--parse-double-p "314"))
  (should (jdee--parse-double-p "-314"))
  (should (jdee--parse-double-p "-3.14_15"))
  (should (jdee--parse-double-p "-314_555.7"))
  (should (jdee--parse-double-p "-314_555.7_8")))

(ert-deftest jdee--parse-char-p-test ()
  (should (jdee--parse-char-p "'a'"))
  (should-not (jdee--parse-char-p "''"))
  (should-not (jdee--parse-char-p "'a")))

(ert-deftest jdee--parse-string-p-test ()
  (should (jdee--parse-string-p "\"\""))
  (should (jdee--parse-string-p "\"abc\""))
  (should-not (jdee--parse-string-p "\"abc")))

(provide 'jdee-parse-test)
;;; jdee-parse-test.el ends here
