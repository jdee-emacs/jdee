;;; Package --- Tests of jdee in-house parsing
;;; Commentary:
;;; Code:

(require 'ert)
(require 'jdee-parse)
(require 'load-relative)

(defconst jdee-test-project-1-dir (relative-expand-file-name "../../jdee-test/project-1/")
  "Directory for project-1 test code")
(defconst jdee-test-this-dir (relative-expand-file-name "./")
  "Directory containing this test code")

(defmacro jdee-test-with-jdee-buffer (content directory &rest body)
  "Evaluate BODY in a temporary buffer with CONTENT.
If DIRECTORY is non-nil, it used as the current directory for the test."
  (declare (debug t)
           (indent 2))
  `(with-temp-buffer
     (insert ,content)
     (let ((jdee-mode-hook nil)
           (java-mode-hook nil)
           (c-mode-common-hook nil)
           (prog-mode-hook nil)
           (buffer-file-name (concat
                              (if ,directory
                                  (concat ,directory "/")
                                jdee-test-this-dir)
                              "Temp.java")))
       (jdee-mode)
       (goto-char (point-min))
       ,@body)))

(defmacro jdee-test-with-existing-class (class &rest body)
  "Fake CLASS being defined and run BODY"
  (declare (debug t)
           (indent 1))
  `(let ((advice-name (concat "class-exists-" ,class)))
     (unwind-protect
       (progn
        ;; Add advice to make the class be defined
        (advice-add
         'jdee-parse-class-exists
          :before-until
          (lambda (name)
            (string-equal name ,class))
          '((name . advice-name)))
        ,@body)
       ;; When done, remove the advice
       (advice-remove #'jdee-parse-class-exists advice-name))))

(ert-deftest jdee-parse-find-decl-same-name ()
  "Test that `jdee-parse-find-declaration-of' works when the variable and the
class have the same name, but different spellings."

  (jdee-test-with-jdee-buffer
      ;; Insert a basic class
      "class Testing {
 private String string;
}"
      nil

    ;; Check that we find it, looking forward and backwards
    (dolist (spot (list (point-max) (point-min)))
      (goto-char spot)

      (let ((start (jdee-parse-find-declaration-of "string")))
        (should start)
        (goto-char start)
        (message "Declaration: %s" (buffer-substring-no-properties start (point-max)))
        (should (looking-at "String string"))))))



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

(ert-deftest jdee-parse-method-from-class ()
  "Completion of 'static_method().'"

  (message "running jdee-parse-method-from-class")
  (jdee-test-with-jdee-buffer
      "class Testing {
private static String hello() { return \"Hello\"; }


}"
      nil

    (jdee-test-with-existing-class "Testing"
      ;; Put the class-info into the cache
      (let ((jdee-complete-classinfo-cache
             '(("Testing"
                ;; Public
                ((nil
                  (("Testing" nil)) ;; Constructors
                  (("hello" "java.lang.String" nil)) ;; Methods
                  nil)
                 (nil nil nil nil) ;; Protected
                 (nil nil nil nil) ;; Package
                 (nil nil nil nil) ;; Private
                )))))
        (search-forward "hello") ;; Parse only checks classes that the point is inside
        (should (equal (jdee-parse-eval-type-of "hello().") "java.lang.String"))))))

(ert-deftest jdee-parse-split-line-style ()
  "Parsing of an expression split over multiple lines:
    foo().
    bar().
    b"

  (message "Running jdee-parse-split-line-style")

  (jdee-test-with-jdee-buffer
      "class Testing {
             Testing() {
               foo().
               bar().
               b
      }"
      nil

    (goto-char (point-min))
    (search-forward-regexp "b$")
    (end-of-line)

    (let ((pair (jdee-parse-java-variable-at-point)))
      (should (string-equal (car pair) "foo().bar"))
      (should (string-equal (cadr pair) "b")))))


(provide 'jdee-parse-test)
;;; jdee-parse-test.el ends here


