;;; Package --- Summary
;;; Commentary:
;;; Code:

(require 'ert)
(require 'jdee-live)

(defconst jdee-live-sample-dir (relative-expand-file-name "../../jdee-sample/"))
(defconst jdee-live-test-dir (relative-expand-file-name "../../jdee-test/"))

(defmacro with-apparent-file (file &rest body)
  "Make it appear that we are currently editing FILE from the sample directory.
Execute BODY then ensure that the nREPL server is not running.
This does not actually load the file or set the major mode, but
sets variables so that the path operations think we are editting
the file."
  (declare (indent 1)
           (debug (sexp body)))
  ;; Fake the current buffer being in the sample directory"
  `(let ((buffer-file-name (concat jdee-live-sample-dir ,file)))
     (should (file-exists-p buffer-file-name))
     (unwind-protect
         (progn ,@body)
       ;; Shutdown the server, and ignore that we are asked questions
       (cl-letf (((symbol-function #'y-or-n-p) (lambda (prompt) t)))
         (jdee-live-stop-nrepl)))))

(defmacro with-apparent-test-file (file &rest body)
  "Make it appear that we are currently editing FILE from the jdee-test directory.
Execute BODY then ensure that the nREPL server is not running.
This does not actually load the file or set the major mode, but
sets variables so that the path operations think we are editting
the file."
  (declare (indent 1)
           (debug (sexp body)))
  ;; Fake the current buffer being in the test directory"
  `(let ((buffer-file-name (concat jdee-live-test-dir ,file)))
     (should (file-exists-p buffer-file-name))
     (unwind-protect
         (progn ,@body)
       ;; Shutdown the server, and ignore that we are asked questions
       (cl-letf (((symbol-function #'y-or-n-p) (lambda (prompt) t)))
         (jdee-live-stop-nrepl)))))



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


(ert-deftest test-jdee-live-dependenices-in-classpath ()
  "Test that the classpath contain jars for the dependencies"

  (with-apparent-test-file "project-2/src/main/java/org/jdee/Project2.java"
    ;; Make sure this is no prj.el.  When we no longer support prj.el, this can
    ;; go away.
    (should-not (jdee-find-project-file "."))

    (let ((classpath (jdee-get-global-classpath)))
      (should classpath)
      ;; Should contain the project-1 jar
      (should (cl-find-if (lambda (path) (string-match "project-1-2.0.jar" path))
                          classpath)))))

(ert-deftest test-jdee-live-parent-child-in-child ()
  "Test that we find the parent and no children when in a project with only a parent project"
  (with-apparent-test-file "project-2/src/main/java/org/jdee/Project2.java"
    (let ((parent (jdee-live-sync-request:parent-path))
          (children (jdee-live-sync-request:child-paths)))
      (should (string-equal (concat parent "/") jdee-live-test-dir))
      (should-not children))))

(ert-deftest test-jdee-live-parent-child-in-parent ()
  "Test that we find no parent and the children when in a project with only a child projects"
  (with-apparent-test-file "pom.xml"
    (let ((parent (jdee-live-sync-request:parent-path))
          (children (jdee-live-sync-request:child-paths)))
      (should-not parent)
      (should (member "project-1" children))
      (should (member "project-2" children))
      (should (eq 2 (length children))))))

(ert-deftest test-jdee-live-sourcepath-multi-module ()
  "Test that we find all the source paths in a multi-module build"
  (with-apparent-test-file "project-2/src/main/java/org/jdee/Project2.java"
                           (let ((source-paths (jdee-get-sourcepath-nrepl)))
                             (should (listp source-paths))
                             (should (eq 4 (length source-paths)))
                             (dolist (type '("main" "test"))
                                     (dolist (project '("1" "2"))
                                             (should (member (concat jdee-live-test-dir "project-" project "/src/" type "/java")
                                                             source-paths)))))))

(provide 'jdee-live-test)
;;; jdee-live-test.el ends here
