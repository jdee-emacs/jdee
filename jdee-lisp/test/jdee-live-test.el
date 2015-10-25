;;; Package --- Summary
;;; Commentary:
;;; Code:

(require 'ert)
(require 'jdee-live)

(defmacro with-apparent-file (file &rest body)
  "Make it appear that we are currently editing FILE from the sample directory.
Execute BODY then ensure that the nREPL server is not running.
This does not actually load the file or set the major mode, but
sets variables so that the path operations think we are editting
the file."
  (declare (indent 1)
           (debug (sexp body)))
  ;; Fake the current buffer being in the sample directory"
  `(let ((buffer-file-name (relative-expand-file-name
                            (concat "../../jdee-sample/" ,file))))
     (unwind-protect
        (progn ,@body)
       ;; Shutdown the server
      (jdee-live-stop-nrepl))))


(ert-deftest test-jdee-live-no-server ()
  "Test no server is found by default"
  (with-apparent-file "pom.xml"
    (should-not (jdee-live-connected-p))))

(ert-deftest test-jdee-live-start-server ()
  "Test we can start the server"
  (with-apparent-file "src/main/java/uk/org/russet/App.java"
    (jdee-live-jack-in)
    (should (jdee-live-connected-p))))

(ert-deftest test-jdee-live-classpath ()
  "Test we can get the classpath with just a pom.xml"

  (with-apparent-file "src/main/java/uk/org/russet/App.java"
    ;; Make sure this is no prj.el.  When we no longer support prj.el, this can
    ;; go away.
    (should-not (jdee-find-project-file "."))

    ;; TODO: Figure out the difference between jdee-global-classpath and
    ;; jdee-get-global-classpath, and why both exist.
    (let ((classpath (jdee-get-global-classpath)))
      (should classpath)
      ;; Should contain target/classes
      (should (cl-find-if (lambda (path) (string-match "/target/classes$" path)) classpath))
      ;; Should contain junit jar
      (should (cl-find-if (lambda (path) (string-match "/junit-[0-9.]*\\.jar$" path)) classpath)))))




(provide 'jdee-live-test)
;;; jdee-live-test.el ends here
