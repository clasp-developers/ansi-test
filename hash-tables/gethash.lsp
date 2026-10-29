;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Fri Nov 28 06:05:21 2003
;;;; Contains: Tests of GETHASH



;;; Most testing of GETHASH is in test-hash-table-1 in hash-table-aux.lsp

(deftest gethash.1
  (gethash 'x (make-hash-table) 'y)
  y nil)

(deftest gethash.2
  (gethash nil (make-hash-table) 'a)
  a nil)

(deftest gethash.3
  (gethash nil (make-hash-table) 'a)
  a nil)

(deftest gethash.4
  (multiple-value-bind (value present)
      (gethash 'a (let ((table (make-hash-table)))
                    (setf (gethash 'a table) 'b)
                    table))
    (values value (notnot present)))
  b t)

(deftest gethash.5
  (let ((table (make-hash-table))
        (i 0))
    (values
     (setf (gethash 'x table (incf i)) 'y)
     i
     (gethash 'x table)))
  y 1 y)

(deftest gethash.order.1
  (let ((i 0) x y
        (table (make-hash-table)))
    (setf (gethash 'a table) 'b)
    (values
     (gethash (progn (setf x (incf i)) 'a)
              (progn (setf y (incf i)) table))
     i x y))
  b 2 1 2)

(deftest gethash.order.2
  (let ((i 0) x y z
        (table (make-hash-table)))
    (setf (gethash 'a table) 'b)
    (values
     (gethash (progn (setf x (incf i)) 'a)
              (progn (setf y (incf i)) table)
              (progn (setf z (incf i)) 'missing))
     i x y z))
  b 3 1 2 3)

(deftest gethash.order.3
  (let ((i 0) x y
        (table (make-hash-table)))
    (values
      (setf (gethash (progn (setf x (incf i)) 'a)
                     (progn (setf y (incf i)) table))
            'b)
      i x y
      (gethash 'a table)))
  b 2 1 2 b)

(deftest gethash.order.4
  (let ((i 0) x y z
        (table (make-hash-table)))
    (values
      (setf (gethash (progn (setf x (incf i)) 'a)
                     (progn (setf y (incf i)) table)
                     (setf z (incf i)))
            'b)
      i x y z
      (gethash 'a table)))
  b 3 1 2 3 b)


;;; Tests for 0.0, -0.0 in hash tables

(deftest gethash.zero.1
  (loop for pz in '(0.0s0 0.0f0 0.0d0 0.0l0)
        for nz = (- pz)
        for result = (let ((table (make-hash-table :test 'eq)))
                       (list
                        (setf (gethash pz table) :x)
                        (gethash pz table)
                        (gethash nz table)
                        (setf (gethash nz table) :y)
                        (gethash pz table)
                        (gethash nz table)))
        unless (or (eql pz nz)
                   (equal result '(:x :x nil :y :x :y)))
        collect (list pz nz result))
  nil)

(deftest gethash.zero.2
  (loop for pz in '(0.0s0 0.0f0 0.0d0 0.0l0)
        for nz = (- pz)
        for result = (let ((table (make-hash-table :test 'eql)))
                       (list
                        (setf (gethash pz table) :x)
                        (gethash pz table)
                        (gethash nz table)
                        (setf (gethash nz table) :y)
                        (gethash pz table)
                        (gethash nz table)))
        unless (or (eql pz nz)
                   (equal result '(:x :x nil :y :x :y)))
        collect (list pz nz result))
  nil)

(deftest gethash.zero.3
  (loop for pz in '(0.0s0 0.0f0 0.0d0 0.0l0)
        for nz = (- pz)
        for result = (let ((table (make-hash-table :test 'equal)))
                       (list
                        (setf (gethash pz table) :x)
                        (gethash pz table)
                        (gethash nz table)
                        (setf (gethash nz table) :y)
                        (gethash pz table)
                        (gethash nz table)))
        unless (or (eql pz nz)
                   (equal result '(:x :x nil :y :x :y)))
        collect (list pz nz result))
  nil)

(deftest gethash.zero.4
  (loop for pz in '(0.0s0 0.0f0 0.0d0 0.0l0)
        for nz = (- pz)
        for result = (let ((table (make-hash-table :test 'equalp)))
                       (list
                        (setf (gethash pz table) :x)
                        (gethash pz table)
                        (gethash nz table)
                        (setf (gethash nz table) :y)
                        (gethash pz table)
                        (gethash nz table)))
        unless (or (eql pz nz)
                   (equal result '(:x :x :x :y :y :y)))
        collect (list pz nz result))
  nil)

;;; Tests for float-ratio and float-integer equivalence (#'= -> EQUALP)

(deftest gethash.num=.1 ;; ratio
  (loop for fv in '(0.5s0 0.5f0 0.5d0 0.5l0)
        for rv = (rationalize fv)
        for result = (let ((table (make-hash-table :test 'equalp)))
                       (list
                        (setf (gethash fv table) :x)
                        (gethash fv table)
                        (gethash rv table)
                        (setf (gethash rv table) :y)
                        (gethash fv table)
                        (gethash rv table)))
        unless (equal result '(:x :x :x :y :y :y))
          collect (list fv rv result))
  nil)

(deftest gethash.num=.2 ;; fixnum
  (loop for fv in '(5.0s0 5.0f0 5.0d0 5.0l0)
        for rv = (rationalize fv)
        for result = (let ((table (make-hash-table :test 'equalp)))
                       (list
                        (setf (gethash fv table) :x)
                        (gethash fv table)
                        (gethash rv table)
                        (setf (gethash rv table) :y)
                        (gethash fv table)
                        (gethash rv table)))
        unless (equal result '(:x :x :x :y :y :y))
          collect (list fv rv result))
  nil)

(deftest gethash.num=.3 ;; bignum
  (loop for fv in (list most-positive-short-float
                        most-positive-single-float
                        most-positive-double-float
                        most-positive-long-float)
        for rv = (rationalize fv)
        for result = (let ((table (make-hash-table :test 'equalp)))
                       (list
                        (setf (gethash fv table) :x)
                        (gethash fv table)
                        (gethash rv table)
                        (setf (gethash rv table) :y)
                        (gethash fv table)
                        (gethash rv table)))
        unless (equal result '(:x :x :x :y :y :y))
          collect (list fv rv result))
  nil)

(deftest gethash.num=.4 ;; #c(f 0.0) = f
  (loop for fv in (list 1.0s0 1.0f0 1.0d0 1.0l0)
        for rv = (rationalize fv)
        for fc = (complex fv (float 0.0 fv))
        for result = (let ((table (make-hash-table :test 'equalp)))
                       (list
                        (setf (gethash fv table) :x)
                        (gethash fv table)
                        (gethash rv table)
                        (gethash fc table)
                        (setf (gethash rv table) :y)
                        (gethash fv table)
                        (gethash rv table)
                        (gethash fc table)
                        (setf (gethash fc table) :z)
                        (gethash fv table)
                        (gethash rv table)
                        (gethash fc table)))
        unless (equal result '(:x :x :x :x :y :y :y :y :z :z :z :z))
          collect (list fv rv fc result))
  nil)

(deftest gethash.num=.5 ;; #c(r1 r2) = #c(f1 f2)
  (loop for fv in (list 1.0s0 1.0f0 1.0d0 1.0l0)
        for rv = (rationalize fv)
        for fc = (complex fv 42)
        for rc = (complex rv 42)
        for result = (let ((table (make-hash-table :test 'equalp)))
                       (list
                        (setf (gethash fc table) :x)
                        (gethash fc table)
                        (gethash rc table)
                        (setf (gethash rc table) :y)
                        (gethash fc table)
                        (gethash rc table)))
        unless (equal result '(:x :x :x :y :y :y))
          collect (list fc rc result))
  nil)


(deftest gethash.num=.6 ;; different float types
  ;; We use F=3*(expt 2 -20) to ensure different bit patterns.
  (loop with rv = (* 3 (expt 2 -20))
        with sf = (float rv 1.0s0)
        with ff = (float rv 1.0f0)
        with df = (float rv 1.0d0)
        with lf = (float rv 1.0l0)
        for f1 in (list sf sf sf    ff ff ff    df df df    lf lf lf)
        for f2 in (list ff df lf    sf df lf    sf ff lf    sf ff df)
        for result = (let ((table (make-hash-table :test 'equalp)))
                       (list
                        (setf (gethash f1 table) :x)
                        (gethash f1 table)
                        (gethash f2 table)
                        (setf (gethash f2 table) :y)
                        (gethash f1 table)
                        (gethash f2 table)))
        unless (equal result '(:x :x :x :y :y :y))
          collect (list f1 f2 result))
  nil)

;;; Unrepresentable as binary float (non-dyadic ratio).
(deftest gethash.num/=.1 ;; ratio
  (loop with rv = 1/3
        for fv in (list (float rv 1.0s0)
                        (float rv 1.0f0)
                        (float rv 1.0d0)
                        (float rv 1.0l0))
        for result = (let ((table (make-hash-table :test 'equalp)))
                       (list
                        (setf (gethash fv table) :x)
                        (setf (gethash rv table) :y)
                        (gethash fv table)
                        (gethash rv table)
                        (setf (gethash rv table) :z)
                        (gethash fv table)
                        (gethash rv table)))
        unless (equal result '(:x :y :x :y :z :x :z))
          collect (list rv fv result))
  nil)

;;; Exceed mantissa.
(deftest gethash.num/=.2 ;; integer
  (loop for pv in '(1.0s0 1.0f0 1.0d0 1.0l0)
        for precision = (float-precision pv)
        for rv = (1- (expt 2 (1+ precision)))
        for fv = (float rv pv)
        for result = (let ((table (make-hash-table :test 'equalp)))
                       (list
                        (setf (gethash fv table) :x)
                        (setf (gethash rv table) :y)
                        (gethash fv table)
                        (gethash rv table)
                        (setf (gethash rv table) :z)
                        (gethash fv table)
                        (gethash rv table)))
        unless (equal result '(:x :y :x :y :z :x :z))
          collect (list pv fv rv result))
  nil)

;;; Different rounding.
(deftest gethash.num/=.3 ;; different float types
  ;; We use F=1/3 because binary float can't handle an exact value.
  (loop with rv = 1/3
        with sf = (float rv 1.0s0)
        with ff = (float rv 1.0f0)
        with df = (float rv 1.0d0)
        with lf = (float rv 1.0l0)
        for f1 in (list sf sf sf    ff ff ff    df df df    lf lf lf)
        for f2 in (list ff df lf    sf df lf    sf ff lf    sf ff df)
        for result = (let ((table (make-hash-table :test 'equalp)))
                       (list
                        (setf (gethash f1 table) :x)
                        (setf (gethash f2 table) :y)
                        (gethash f1 table)
                        (gethash f2 table)))
        unless (or (= f1 f2) ; i.e sf===ff
                   (equal result '(:x :y :x :y)))
          collect (list f1 f2 result))
  nil)

;;;; Error tests

(deftest gethash.error.1
  (signals-error (gethash) program-error)
  t)

(deftest gethash.error.2
  (signals-error (gethash 'foo) program-error)
  t)

(deftest gethash.error.3
  (signals-error (gethash 'foo (make-hash-table) nil nil) program-error)
  t)
