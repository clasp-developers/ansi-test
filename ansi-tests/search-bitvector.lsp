;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Sun Aug 25 13:06:54 2002
;;;; Contains: Tests for SEARCH on vectors

(in-package :cl-test)

(deftest search-bitvector.1
  (let ((target *searched-bitvector*)
	(pat #(a)))
    (loop for i from 0 to (1- (length target))
	  for tail on target
	  always
	  (let ((pos (search pat tail)))
	    (search-check pat tail pos))))
  t)

(deftest search-bitvector.2
  (let ((target *searched-bitvector*)
	(pat #(a)))
    (loop for i from 1 to (length target)
	  always
	  (let ((pos (search pat target :end2 i :from-end t)))
	    (search-check pat target pos :end2 i :from-end t))))
  t)

(deftest search-bitvector.3
  (let ((target *searched-bitvector*))
    (loop for pat in *pattern-subbitvectors*
	  for pos = (search pat target)
	  unless (search-check pat target pos)
	  collect pat))
  nil)

(deftest search-bitvector.4
  (let ((target *searched-bitvector*))
    (loop for pat in *pattern-subbitvectors*
	  for pos = (search pat target :from-end t)
	  unless (search-check pat target pos :from-end t)
	  collect pat))
  nil)

(deftest search-bitvector.5
  (let ((target *searched-bitvector*))
    (loop for pat in *pattern-subbitvectors*
	  for pos = (search pat target :start2 25 :end2 75)
	  unless (search-check pat target pos :start2 25 :end2 75)
	  collect pat))
  nil)

(deftest search-bitvector.6
  (let ((target *searched-bitvector*))
    (loop for pat in *pattern-subbitvectors*
	  for pos = (search pat target :from-end t :start2 25 :end2 75)
	  unless (search-check pat target pos :from-end t
			       :start2 25 :end2 75)
	  collect pat))
  nil)

(deftest search-bitvector.7
  (let ((target *searched-bitvector*))
    (loop for pat in *pattern-subbitvectors*
	  for pos = (search pat target :start2 20)
	  unless (search-check pat target pos :start2 20)
	  collect pat))
  nil)

(deftest search-bitvector.8
  (let ((target *searched-bitvector*))
    (loop for pat in *pattern-subbitvectors*
	  for pos = (search pat target :from-end t :start2 20)
	  unless (search-check pat target pos :from-end t
			       :start2 20)
	  collect pat))
  nil)

(deftest search-bitvector.9
  (let ((target *searched-bitvector*))
    (loop for pat in (mapcar #'(lambda (x)
				 (map 'vector
				      #'(lambda (y)
					  (sublis '((a . 2) (b . 3)) y))
				      x))
			     *pattern-sublists*)
	  for pos = (search pat target :start2 20 :key #'evenp)
	  unless (search-check pat target pos :start2 20 :key #'evenp)
	  collect pat))
  nil)

(deftest search-bitvector.10
  (let ((target *searched-bitvector*))
    (loop for pat in (mapcar #'(lambda (x)
				 (map 'vector
				      #'(lambda (y)
					  (sublis '((a . 2) (b . 3)) y))
				      x))
			     *pattern-sublists*)
	  for pos = (search pat target :from-end t :start2 20 :key 'oddp)
	  unless (search-check pat target pos :from-end t
			       :start2 20 :key 'oddp)
	  collect pat))
  nil)

(deftest search-bitvector.11
  (let ((target *searched-bitvector*))
    (loop for pat in *pattern-subbitvectors*
	  for pos = (search pat target :start2 20 :test (complement #'eq))
	  unless (search-check pat target pos :start2 20
			       :test (complement #'eq))
	  collect pat))
  nil)

(deftest search-bitvector.12
  (let ((target *searched-bitvector*))
    (loop for pat in *pattern-subbitvectors*
	  for pos = (search pat target :from-end t :start2 20 :test-not #'eq)
	  unless (search-check pat target pos :from-end t
			       :start2 20 :test (complement #'eq))
	  collect pat))
  nil)

(deftest search-bitvector.13
  (let ((target *searched-bitvector*))
    (loop for pat in *pattern-subbitvectors*
	  when (and (> (length pat) 0)
		    (let ((pos (search pat target :start1 1
				       :test (complement #'eq))))
		      (not (search-check pat target pos
					 :start1 1
					 :test (complement #'eq)))))
	  collect pat))
  nil)

(deftest search-bitvector.14
  (let ((target *searched-bitvector*))
    (loop for pat in *pattern-subbitvectors*
	  when (let ((len (length pat)))
		 (and (> len 0)
		      (let ((pos (search pat target :end1 (1- len)
					 :test (complement #'eq))))
		      (not (search-check pat target pos
					 :end1 (1- len)
					 :test (complement #'eq))))))
	  collect pat))
  nil)
