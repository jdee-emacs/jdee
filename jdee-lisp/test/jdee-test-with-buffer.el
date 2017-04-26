;;; Package --- Framework for defining test source in-line
;;; Commentary:
;;;    Ths defines some macros that allow tests to be created that specify the
;;; contents of a java buffer inline.  This keeps the test and the java source
;;; nearby and prevents a proliferation of files, some of which may not compile.
;;;
;;; Code:

(require 'load-relative)


(defconst jdee-test-project-1-dir (relative-expand-file-name "../../jdee-test/project-1/")
  "Directory for project-1 test code.")
(defconst jdee-test-this-dir (relative-expand-file-name "./")
  "Directory containing this test code.")

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

(provide 'jdee-test-with-buffer)
;;; jdee-test-with-buffer.el ends here


