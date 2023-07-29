;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Sat Mar 14 10:13:21 1998
;;;; Contains: CL test case package definition

(defvar extrinsic-symbols nil)

(let* ((name :cl-test)
       (pkg (find-package name)))
  (unless pkg
    (setq pkg (make-package name :use '())))
  (let ((*package* pkg))
    (import (list* 'common-lisp-user::compile-and-load
                   'common-lisp-user::compile-and-load*
                   extrinsic-symbols))
    (shadow (list* '#:handler-case
                   '#:handler-bind
                   extrinsic-symbols))
    (use-package '(:cl :regression-test))
    (export (mapcar #'intern
                    (mapcar #'symbol-name
                            '(#:random-from-seq #:random-case #:coin
                              #:random-permute #:*universe* #:*mini-universe*
                              #:*cl-symbols*
                              #:signals-error #:typef)))))
  (let ((s (find-symbol "QUIT" "CL-USER")))
    (when s (import s :cl-test))))


