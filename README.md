# jdee
The JDEE is an add-on software package that turns Emacs into a comprehensive system for creating, editing, debugging, and documenting Java applications.

# Build options
1. Available Ant targets: ```ant -p```
2. Run unit tests: ```ant test```
3. Run JUnit tests: ```ant test-java```
4. Build project: ```ant bindist```
  - then in your .emacs add:
  ```emacs-lisp
    (add-to-list 'load-path "/path/to/jdee/dist/jdee-2.4.2/lisp")
    (load "jde-autoload")
  ```
