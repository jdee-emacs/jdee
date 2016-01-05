;;; Package --- Tests of jdee in-house parsing
;;; Commentary:
;;; Code:

(require 'ert)
(require 'jdee-parse)

(ert-deftest jdee-parse-find-decl-same-name ()
  "Test that `jdee-parse-find-declaration-of' works when the variable and the
class have the same name, but different spellings."

  (with-temp-buffer
    ;; Insert a basic class
    (insert "class Testing {
 private String string;
}")

    ;; Check that we find it, looking forward and backwards
    (dolist (spot (list (point-max) (point-min)))
      (goto-char spot)

      (let ((start (jdee-parse-find-declaration-of "string")))
        (should start)
        (goto-char start)
        (message "Declaration: %s" (buffer-substring-no-properties start (point-max)))
        (should (looking-at "String string"))))))


(provide 'jdee-parse-test)
;;; jdee-parse-test.el ends here



