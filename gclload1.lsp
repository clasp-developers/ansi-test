#+ecl (si::package-lock (find-package "COMMON-LISP") nil)
#+ecl (compile nil '(lambda () nil))
#+ecl (setq c:*suppress-compiler-warnings* t
            c:*suppress-compiler-notes*    t
            c:*suppress-compiler-messages* t)
#+:armedbear (require 'pprint)
#+cmu
(progn
  (setq ext:*gc-verbose* nil)
  ;; Set *default-pathname-defaults* to include the full path to this
  ;; file.  This is needed for the tests so that they'll be loaded
  ;; correctly from the subdirectories.
  (setf *default-pathname-defaults*
	(make-pathname :name nil :type nil :defaults *load-truename*)))

#+gcl (setq compiler:*suppress-compiler-notes* t
            compiler:*suppress-compiler-warnings* t
            compiler:*compile-verbose* nil
            compiler:*compile-print* nil)

#+lispworks (setq compiler::*compiler-warnings* nil)
#+lispworks (make-echo-stream *standard-input* *standard-output*)
#+clisp (setq custom::*warn-on-floating-point-contagion* nil)

;;; Configure logical pathnames
(defvar *aux-dir*
  (merge-pathnames "auxiliary/"
                   (make-pathname
                    :directory
                    (pathname-directory *load-truename*))))

(let (*load-verbose* *load-print* *compile-verbose* *compile-print*)
  (load "compile-and-load.lsp"))

(let (*load-verbose* *load-print* *compile-verbose* *compile-print*)
  (load "rt-package.lsp")
  (compile-and-load "rt.lsp")
  (load "cl-test-package.lsp")
  (in-package :cl-test)
  ;; Load up customizations.  It's ok if the file doesn't exist.
  #+cmucl
  (load "rt-cmucl.lsp" :if-does-not-exist nil)
  (compile-and-load* "ansi-aux-macros.lsp")
  (handler-bind
   #-sbcl ()
   #+sbcl ((sb-ext:code-deletion-note #'muffle-warning))
   (load "universe.lsp"))
  (compile-and-load* "random-aux.lsp")
  (compile-and-load* "ansi-aux.lsp")
  
  (load "cl-symbol-names.lsp")
  (load "notes.lsp"))

(setq *compile-verbose* nil
      *compile-print* nil
      *load-verbose* nil)

