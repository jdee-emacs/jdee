;;; jdee-backend -- Facade to JVM backend

;;; Commentary:
;; This is going to be a Facade to a JVM backed.
;; Currently it's only a set of functions extracted from all over the project,
;; but towards the Facade.

;;; Code:

(require 'jdee-classpath)
(require 'beanshell)
(require 'jdee-bsh)

(defun jdee-backend-browse-class (fqn)
  "Browse class FQN in backend's buffer."
  (bsh-eval
   (oref-default 'jdee-bsh the-bsh)
   (format "browseClass(\"%s\");" fqn)))

(defun jdee-backend-explore-class (class-to-open)
  "Explore class CLASS-TO-OPEN."
  (bsh-eval
   (oref-default 'jdee-bsh the-bsh)
   (concat "exploreClass(\"" class-to-open "\");")))

(defun jdee-backend-get-ant-start-server-command (command)
  "Start the Ant Server with given COMMAND."
  (concat "jde.util.AntServer.start(\"" command "\");" "\n"))

(defun jdee-backend-class-exists-p (fqc)
  "Return t when class FQC exists."
  (jdee-jeval-r
   (concat "jde.util.JdeUtilities.classExists(\"" fqc "\");")))

(defun jdee-backend-jeval-classname (fmt interface-name &optional eval-return)
  "Try jdee-jeval on the command derived from (format FMT INTERFACE-NAME),
if that fails (as it will when INTERFACE-NAME is an inner-class name),
then try after replacing INTERFACE-NAME with (jdee-dollar-name INTERFACE-NAME).

If EVAL-RETURN is t, then return (jdee-jeval ... t), else return (read (jdee-jeval ...))"
  (cl-flet ((jeval (name)
                   (if eval-return
                       (jdee-jeval (format fmt name) t)
                     (read (jdee-jeval (format fmt name))))))
    (let ((code (jeval interface-name)) dollar-name)
      (if (and code (eq (car code) 'error)
               (setq dollar-name (jdee-dollar-name interface-name))
               ;; recurse as long as '.'s are changing:
               (not (string-equal dollar-name interface-name)))
          ;; try again with dollar-name
          (jdee-backend-jeval-classname fmt dollar-name eval-return)
        code))))

;; To retrieve all the possible completions, it uses the java code in
;; jde.util.Completion.getClassInfo(), called by beanshell.  That
;; need the class to be compiled (but that's not worst than an etag
;; call).
;; Known bugs/problems :
;; - The first call to the bsh function is bugged, and part of the
;; output is trashed.  Starting the bsh before completing, or just
;; ignoring the first error and call the completion again, work fine.
;; - Due to the way the JVM works, it is not possible to explicitly
;; unload a class.  So, if major changes are done in a class, the
;; beanshell must be restarted in order to reload the class.
(defun jdee-backend-get-class-info (name)
  "Return the class info list for the class NAME.
Possibly it's short java name.  This list contains lists of elements,
which car is a possible completion, and the cdr gives additional
informations on the car."
  (bsh-eval
   (oref-default 'jdee-bsh the-bsh)
   (concat "jde.util.Completion.getClassInfo(\"" name "\");")))

(defun jdee-backend-get-qualified-name (unqualified-class)
  "Return a list containing all qualified name for UNQUALIFIED-CLASS."
  (jdee-jeval-r
   (concat "jde.util.JdeUtilities.getQualifiedName(\""
           unqualified-class
           "\");")))

(defun jdee-backend-get-classinfo-access (name access)
  "Invoke the method jde.util.Completion.getClassInfo(String NAME, int ACCESS)."
  (jdee-jeval-r
   (format "jde.util.Completion.getClassInfo(\"%s\",%d);" name access)))

(defun jdee-backend--get-classinfo-javacode (name import)
  "Return the java code that calls the
jde.util.Completion.getClassInfo function with the short java class
name NAME and the package list IMPORT where to look at."
  (save-excursion
    (concat "{ "
            "String[] lst = new String[" (length import) "];\n"
            (let ((count -1))
              (mapconcat (function (lambda (x)
                                     (setq count (+ 1 count))
                                     (concat "lst[" count "]=\""
                                             (car (nth count import)) "\";\n")))
                         import
                         " "))
            "jde.util.Completion.getClassInfo(\"" name "\",lst);\n"
            "}")))

(defun jdee-backend-get-class-info-for-import (name import)
  "Return the class info list for the class NAME and IMPORT.
Possibly it's short java name.  This list contains lists of elements,
which car is a possible completion, and the cdr gives additional
informations on the car."
  (bsh-eval
   (oref-default 'jdee-bsh the-bsh)
   (jdee-backend--get-classinfo-javacode name import)))

;; TODO unify with below
(defun jdee-backend-load-project-class-list2 ()
  "Update the class list used to resolve class names."
  (when (jdee-bsh-running-p)
    (jdee-jeval (jdee-backend-create-prj-values-str))))

(defun jdee-backend-load-project-class-list ()
  "Update the class list used to resolve class names."
  (if (jdee-bsh-running-p)
      (progn
        (message "Rescanning classes...")
        (jdee-jeval (jdee-backend-create-prj-values-str))
        (jdee-jeval "jde.util.JdeUtilities.updateClassList();")
        (message "Rescanning classes...Complete"))))

(defun jdee-backend-update-class-list (class-dir)
  "Request JVM Backend to update list of classes in CLASS-DIR."
  (jdee-jeval (concat
               "jde.util.JdeUtilities.updateClassList(\""
               class-dir
               "\");")))

(defun jdee-build-path-arg (arg path-list &optional quote symbol)
  "Build a command-line path argument from a list of paths."
  (let ((path (jdee-build-classpath path-list symbol)))
    (if quote
        (setq path (concat "\"" path "\"")))
    (setq path (concat arg " " path))))

(defun jdee-build-classpath-arg (path-list &optional quote symbol)
  "Build a classpath from a list of paths."
  (jdee-build-path-arg "-classpath" path-list quote symbol))

(defun jdee-backend-create-prj-values-str ()
  "Create Java expression that updates the JDEE's class list
to include all the classes on `jdee-global-classpath', if
defined, otherwise the classpath specified by the CLASSPATH
environment variable."
  (let* ((directory-sep-char ?/)  ;; Override NT/XEmacs setting
         (classpath
          (jdee-build-path-arg nil (jdee-get-global-classpath) t 'jdee-global-classpath)))
    (format "jde.util.JdeUtilities.setProjectValues(\"%s\", %s);"
            jdee-current-project
            classpath)))

(defun jdee-backend-launch ()
  "Start JVM backend if it's not rurnnig."
  (if (not (jdee-backend-running-p))
      (bsh-launch (oref-default 'jdee-bsh the-bsh))))

(defun jdee-backend-run ()
  "Start JVM backend."
  (interactive)
  (jdee-bsh-run))

(defun jdee-backend-exit ()
  "Stop JVM backend."
  (interactive)
  (jdee-bsh-exit))

(defun jdee-backend--start-bsh ()
  "Start Beanshell server if not running."
  (if (not (jdee-bsh-running-p))
      (progn
        (bsh-launch (oref-default 'jdee-bsh the-bsh))
        (bsh-eval (oref-default 'jdee-bsh the-bsh)
                  (jdee-backend-create-prj-values-str)))))

(defun jdee-backend-compile (arg-array buffer)
  "Compile ARG-ARRAY in BUFFER."
  (jdee-backend--start-bsh)
  (bsh-buffer-eval
   (oref-default 'jdee-bsh the-bsh)
   (concat (format "jde.util.CompileServer.compile(%s);" arg-array) "\n")
   buffer))

(defun jdee-backend-compile-eclipse (path arg-array buffer)
  "Add PATH to class path and compile ARG-ARRAY in BUFFER using Eclipse Compiler."
  (jdee-backend--start-bsh)
  (jdee-backend-add-to-class-path path)
  (bsh-buffer-eval
   (oref-default 'jdee-bsh the-bsh)
   (concat
    (format
     "if ((new org.eclipse.jdt.internal.compiler.batch.Main(new java.io.PrintWriter(System.out), new java.io.PrintWriter(System.out), true, null, null)).compile(%s)) { print (\"0\");} else {print (\"1\");};"
     arg-array)
    "\n")
   buffer))

(defun jdee-backend-add-to-class-path (path)
  "Add PATH to compilation class path."
  (bsh-eval (oref-default 'jdee-bsh the-bsh)
            (format "addClassPath(\"%s\");" path)))

(defun jdee-backend-is-ancestor-of (ancestor-fqn fqn)
  "Return t when ANCESTOR-FQN is ancestor of FQN."
  (jdee-jeval-r
   (format "jde.util.Completion.isAncestorOf(%S,%S);" ancestor-fqn fqn)))

(defun jdee-backend-parse-file (filename)
  "Parse file FILENAME and return parse errors if any."
  (jdee-jeval-r
   (concat "jde.parser.ParserMain.parseFile(\"" filename "\");")))

(defun jdee-backend-get-component-type-name (array-class)
  "Return name of component type for given ARRAY-CLASS."
  (jdee-jeval
   (concat "System.out.println( Class.forName(\""
           array-class
           "\").getComponentType().getName()) ;")))

(defun jdee-backend-get-abstract-class-imports ()
  "Return list of imported classes."
  (jdee-jeval-r
   "jde.wizards.AbstractClassFactory.getImportedClasses();"))

(defun jdee-backend-get-interface-imports ()
  "Return list of imported classes."
  (jdee-jeval-r
   "jde.wizards.InterfaceFactory.getImportedClasses();"))

(defun jdee-backend-get-event-source-imports ()
  "Return list of imported classes."
  (jdee-jeval-r
   "jde.wizards.EventSourceFactory.getImportedClasses();"))

(defun jdee-backend-get-delegate-imports ()
  "Return list of imported classes."
  (jdee-jeval-r
   "jde.wizards.DelegateFactory.getImportedClasses();"))

(defun jdee-backend-get-method-override-imports ()
  "Return list of imported classes."
  (jdee-jeval-r
   "jde.wizards.MethodOverrideFactory.getImportedClasses();"))

(defun jdee-backend-maket-abstract-class-expr (class-name)
  "Return abstract class expression for CLASS-NAME."
  (jdee-backend-jeval-classname
   "jde.wizards.AbstractClassFactory.makeAbstractClassExpression(\"%s\", true);"
   class-name))

(defun jdee-backend-make-interface-expr (interface-name)
  "Return interface expression for interface INTERFACE-NAME."
  (jdee-backend-jeval-classname
   "jde.wizards.InterfaceFactory.makeInterfaceExpression(\"%s\",true);"
   interface-name))

(defun jdee-backend-make-event-source-expr (interface-name)
  "Return event source expression for interface INTERFACE-NAME."
  (jdee-backend-jeval-classname
   "jde.wizards.EventSourceFactory.makeEventSourceSupportExpression(\"%s\", true);"
   interface-name))

(defun jdee-backend-make-method-skeleton-expr (variant)
  "Return method skeletor expression for VARIANT."
  (jdee-jeval-r
   (concat
    "jde.wizards.MethodOverrideFactory.getMethodSkeletonExpression("
    variant ");")))

(defun jdee-backend-get-candidate-signatures (qualified-name method-name)
  "Return candidate signatures for QUALIFIED-NAME and METHOD-NAME."
  (let ((fmt (concat
              "jde.wizards.MethodOverrideFactory.getCandidateSignatures"
              "(\"%s\",\"" method-name "\");")))
    (jdee-backend-jeval-classname fmt qualified-name t)))

(defun jdee-backend-make-delegator-methods (delegee class-name)
  "Return delegator methods to DELEGEE of CLASS-NAME."
  (let ((fmt (concat
              "jde.wizards.DelegateFactory.makeDelegatorMethods(\""
              delegee "\", \"%s\", true);")))
    (jdee-backend-jeval-classname fmt class-name)))

;;TODO rewrite to elisp
(defun jdee-backend-url-exists-p (url)
  "Return t if URL exists."
  (let ((cmd (format "\
java.net.URLConnection conn = null;
String urlStr = \"%s\";
try {
  URL url = new URL(urlStr);
  conn = url.openConnection();
  conn.getInputStream().close();
  print(\"t\");
} catch(java.net.MalformedURLException e) {
  print(\"(error \\\"Bad URL: \" + urlStr + \"\\\")\");
} catch(java.io.IOException e) {
  String msg = e.toString().replace(\"\\\"\", \"\\\\\\\"\");
  print(\"nil\");
} finally {
  if (conn instanceof HttpURLConnection) conn.disconnect();
}"
                     (jdee-url-name url))))
    (eval (read (jdee-jeval cmd)))))

(defun jdee-backend-running-p ()
  "Return t if JDEE backend is running."
  (jdee-bsh-running-p))

(defun jdee-backend-get-java-version ()
  "Return backend's Java version."
  (jdee-jeval-r "jde.util.JdeUtilities.getJavaVersion();"))

(provide 'jdee-backend)

;;; jdee-backend.el ends here
