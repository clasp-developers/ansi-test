;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Sat Jun 14 05:46:51 2003
;;;; Contains: Tests of KEYWORDP

(in-package :cl-test)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; keywordp

(deftest keywordp.1 (keywordp 'hefalump)   nil)
(deftest keywordp.2 (keywordp 17)          nil)
(deftest keywordp.3 (notnot-mv (keywordp :stream))         t)
(deftest keywordp.4 (notnot-mv (keywordp ':stream))        t)
(deftest keywordp.5 (keywordp nil)         nil)
(deftest keywordp.6 (notnot-mv (keywordp :nil))          t)
(deftest keywordp.7 (keywordp '(:stream))    nil)
(deftest keywordp.8 (keywordp "rest")     nil)
(deftest keywordp.9 (keywordp ":rest")    nil)
(deftest keywordp.10 (keywordp '&body) nil)
;;; This next test was busted.  ::foo is not portable syntax
;;(deftest keywordp.11 (notnot-mv (keywordp ::foo))       t)
(deftest keywordp.12 (keywordp t)          nil)

(deftest keywordp.order.1
  (let ((i 0))
    (values (keywordp (progn (incf i) nil)) i))
  nil 1)

(deftest keywordp.error.1 (classify-error (keywordp)) program-error)
(deftest keywordp.error.2 (classify-error (keywordp :x :x)) program-error)

(deftest keywordp.error.3
  (classify-error (keywordp))
  program-error)

(deftest keywordp.error.4
  (classify-error (keywordp nil nil))
  program-error)

