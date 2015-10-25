;;; package --- Summary
;;; Commentary:
;;; Code:

(require 'ert)
(require 'jdee-live)




(ert-deftest test-jdee-live-no-server ()
  "Test no server is found by default"
  ;; Fake the current buffer being in the sample directory"
  (let ((buffer-file-name (relative-expand-file-name "../../jdee-sample/pom.xml")))
    (should-not (jdee-live-connected-p))))

(ert-deftest test-jdee-live-start-server ()
  "Test we can start the server"
  ;; Fake the current buffer being in the sample directory"
  (let ((buffer-file-name (relative-expand-file-name "../../jdee-sample/src/main/java/uk/org/russet/App.java")))

    (unwind-protect
        (progn
          ;; Start the server
          (jdee-live-jack-in)
          (should (jdee-live-connected-p)))
      ;; Shut down the server
      (jdee-live-stop-server))))




(provide 'jdee-live-test)
;;; jdee-live-test.el ends here
