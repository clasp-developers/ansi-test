;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Wed Feb 11 06:01:55 2004
;;;; Contains: Tests of ATAN

(in-package :cl-test)

(deftest atan.1
  (let ((result (atan 0)))
    (or (eqlt result 0)
	(eqlt result 0.0)))
  t)

(deftest atan.2
  (loop for type in '(short-float single-float double-float long-float)
	for zero = (coerce 0 type)
	unless (eql (atan zero) zero)
	collect type)
  nil)

(deftest atan.3
  (loop for type in '(short-float single-float double-float long-float)
	for zero = (coerce 0 type)
	unless (eql (atan zero 0) zero)
	collect type)
  nil)

(deftest atan.4
  (loop for type in '(short-float single-float double-float long-float)
	for zero = (coerce 0 type)
	unless (eql (atan 0 zero) zero)
	collect type)
  nil)

(deftest atan.5
  (loop for type in '(short-float single-float double-float long-float)
	for zero = (coerce 0 type)
	unless (eql (atan zero zero) zero)
	collect type)
  nil)

(deftest atan.6
  (loop for type in '(short-float single-float double-float long-float)
	for a = (coerce 2000 type)
	for b = (coerce -1000 type)
	collect
	(loop for x = (- (random a) b)
	      for rlist = (multiple-value-list (atan x))
	      for y = (car rlist)
	      repeat 1000
	      unless (and (null (cdr rlist))
			  (typep y type))
	      collect (list x rlist)))
  (nil nil nil nil))

(deftest atan.7
  (loop for type in '(short-float single-float double-float long-float)
	for a = (coerce 2000 type)
	for b = (coerce -1000 type)
	for zero = (coerce 0 type)
	collect
	(loop for x = (- (random a) b)
	      for rlist = (multiple-value-list (atan (complex x zero)))
	      for y = (car rlist)
	      repeat 1000
	      unless (and (null (cdr rlist))
			  (typep y `(complex ,type)))
	      collect (list x rlist)))
  (nil nil nil nil))

(deftest atan.8
  (loop for type in '(short-float single-float double-float long-float)
	for a = (coerce 2000 type)
	for b = (coerce -1000 type)
	for zero = (coerce 0 type)
	collect
	(loop for x = (- (random a) b)
	      for rlist = (multiple-value-list (atan (complex zero x)))
	      for y = (car rlist)
	      repeat 1000
	      unless (and (null (cdr rlist))
			  (typep y `(complex ,type)))
	      collect (list x rlist)))
  (nil nil nil nil))

(deftest atan.9
  (loop for type in '(short-float single-float double-float long-float)
	for a = (coerce 2000 type)
	for b = (coerce -1000 type)
	for zero = (coerce 0 type)
	collect
	(loop for x1 = (- (random a) b)
	      for x2 = (- (random a) b)
	      for rlist = (multiple-value-list (atan (complex x1 x2)))
	      for y = (car rlist)
	      repeat 1000
	      unless (and (null (cdr rlist))
			  (typep y `(complex ,type)))
	      collect (list x1 x2 rlist)))
  (nil nil nil nil))

(deftest atan.10
  (approx= (atan 1) (coerce (/ pi 4) 'single-float))
  t)

(deftest atan.11
  (loop for type in '(short-float single-float double-float long-float)
	collect (approx= (atan (coerce 1 type)) (coerce (/ pi 4) type)))
  (t t t t))

(deftest atan.12
  (approx= (atan -1) (coerce (/ pi -4) 'single-float))
  t)

(deftest atan.13
  (loop for type in '(short-float single-float double-float long-float)
	collect (approx= (atan (coerce -1 type)) (coerce (/ pi -4) type)))
  (t t t t))

;;; FIXME
;;; More accuracy tests here

;;; Error tests

(deftest atan.error.1
  (signals-error (atan) program-error)
  t)

(deftest atan.error.2
  (signals-error (atan 1 1 1) program-error)
  t)

(deftest atan.error.3
  (loop for x in *mini-universe*
	unless (or (numberp x)
		   (eval `(signals-error (atan ',x) type-error)))
	collect x)
  nil)

(deftest atan.error.4
  (loop for x in *mini-universe*
	unless (or (realp x)
		   (eval `(signals-error (atan ',x 1) type-error)))
	collect x)
  nil)

(deftest atan.error.5
  (loop for x in *mini-universe*
	unless (or (realp x)
		   (eval `(signals-error (atan 1 ',x) type-error)))
	collect x)
  nil)




