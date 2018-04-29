;;; Package --- Tests of jdee completion
;;; Commentary:
;;; Code:

(require 'ert)
(require 'jdee-complete)
(load-file
 (expand-file-name "jdee-test-with-buffer.el"
                   (file-name-directory (or load-file-name (buffer-file-name)))))

(ert-deftest jdee-complete-on-newline ()
  "Test that `jdee-complete' works when there is a newline between the previous text and the point."

  (jdee-test-with-jdee-buffer

    ;; Insert a basic class
   "class Testing {

     public String someString() { return \"some\"; }

     public String foo() {
         someString().

         // Point on previous line

     }

}"
      nil

    ;; Goto to line after text
    (goto-char (point-min))
    (search-forward "previous line")
    (forward-line -1)

    ;; Find the variable we are completing
    (let ((pair (jdee-parse-java-variable-at-point)))
      (should (equal '("someString" "") pair)))))

(ert-deftest jdee-complete-inner-enum ()
  "Class lookup of an inner enum"

  (message "Running jdee-complete-inner-enum")

  (jdee-test-with-jdee-buffer
      "class Testing {
       private final String foo;
    }"
      jdee-test-project-1-dir

    (let ((info  (jdee-complete-get-classinfo "uk.org.russet.Outer.InnerEnum")))
      (message "Info %s" info)
      (should info))))


(provide 'jdee-complete-test)
;;; jdee-complete-test.el ends here



