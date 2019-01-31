;;; jdee-open-source-test.el --- Unit tests  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'ert)
(require 'el-mock)

(require 'jdee-open-source)

(ert-deftest test-jdee-remove-type ()
  (should (equal '("List") (jdee-remove-type '("List<String>")))))

(ert-deftest test-jdee-find-class-source-file-no-source-for-class ()
  "Should return `nil' when there's no Java source for given class."
  (let ((class "util.Foo")
        (jdee-sourcepath '("src")))
    (with-mock
      (mock (jdee-expand-wildcards-and-normalize jdee-sourcepath 'jdee-sourcepath)
            => '("/src/text"))
      (stub file-exists-p => nil)
      (should-not (jdee-find-class-source-file class)))))

(ert-deftest test-jdee-find-class-source-file-in-package-dir ()
  "Should return path to source file found in package directory relative
to source path."
  (let ((class "util.Foo")
        (jdee-sourcepath '("src1" "src2"))
        (expanded-paths '("/src1" "/src2"))
        (expected-path "/src2/util/Foo.java"))
    (cl-letf (((symbol-function 'file-exists-p)
               (lambda (filename)
                 (string= filename expected-path)))
              ((symbol-function 'jdee-expand-wildcards-and-normalize)
               (lambda (path symbol)
                 (should (equal path jdee-sourcepath))
                 (should (equal symbol 'jdee-sourcepath))
                 expanded-paths)))
      (should (string= expected-path (jdee-find-class-source-file class))))))

(ert-deftest test-jdee-find-class-source-file-source-in-sourcepath ()
  "Should return path to source file for a class in the current sourcepath dir."
  (let ((class "util.Foo")
        (jdee-sourcepath '("src"))
        (expanded-paths '("/src"))
        (expected-path "/src/Foo.java"))
    (cl-letf (((symbol-function 'file-exists-p)
               (lambda (filename)
                 (string= filename expected-path)))
              ((symbol-function 'jdee-expand-wildcards-and-normalize)
               (lambda (path symbol)
                 (should (equal path jdee-sourcepath))
                 (should (equal symbol 'jdee-sourcepath))
                 expanded-paths)))
      (should (string= expected-path (jdee-find-class-source-file class))))))

(ert-deftest test-jdee-find-class-source-file-archive-existing-buffer ()
  "Should return a buffer of already opened source file."
  (let ((class "util.Foo")
        (jdee-sourcepath '("src" "repo/*.jar"))
        (expanded-paths '("/src" "/repo/foolib.jar"))
        (lib-path "/repo/foolib.jar")
        (expected-name "Foo.java (foolib.jar)"))
    (cl-letf (((symbol-function 'file-exists-p)
               (lambda (filename)
                 (string= filename lib-path)))
              ((symbol-function 'jdee-expand-wildcards-and-normalize)
               (lambda (path symbol)
                 (should (equal path jdee-sourcepath))
                 (should (equal symbol 'jdee-sourcepath))
                 expanded-paths)))
      (with-temp-buffer
        (rename-buffer expected-name)
        (let ((result (jdee-find-class-source-file class)))
          (should (bufferp result))
          (should (string= expected-name (buffer-name result))))))))

(ert-deftest test-jdee-find-class-source-file-in-archive ()
  "Should return a buffer to a file loaded from an archive."
  (let ((class "util.Foo")
        (jdee-sourcepath '("src" "repo/*.jar"))
        (expanded-paths '("/src" "/repo/foolib.jar"))
        (lib-path "/repo/foolib.jar")
        (expected-name "Foo.java (foolib.jar)"))
    (cl-letf (((symbol-function 'jdee-expand-wildcards-and-normalize)
               (lambda (path symbol)
                 (should (equal path jdee-sourcepath))
                 (should (equal symbol 'jdee-sourcepath))
                 expanded-paths))
              ((symbol-function 'file-exists-p)
               (lambda (filename)
                 (string= filename lib-path)))
              ((symbol-function 'archive-extract-by-stdout)
               (lambda (path class-file-name command)
                 (should (string= lib-path path))
                 (should (string= class-file-name "util/Foo.java"))
                 (should (eql command archive-zip-extract))
                 0)))
      (unwind-protect
          (let ((result (jdee-find-class-source-file class)))
            (should (bufferp result))
            (should (string= expected-name (buffer-name result))))
        (kill-buffer expected-name)))))

(provide 'jdee-open-source-test)
;;; jdee-open-source-test.el ends here
