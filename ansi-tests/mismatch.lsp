;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Mon Aug 26 23:55:29 2002
;;;; Contains: Tests for MISMATCH

(in-package :cl-test)

(deftest mismatch-list.1
  (mismatch '() '(a b c))
  0)

(deftest mismatch-list.2
  (mismatch '(a b c d) '())
  0)

(deftest mismatch-list.3
  (mismatch '(a b c) '(a b c))
  nil)

(deftest mismatch-list.4
  (mismatch '(a b c) '(a b d))
  2)

(deftest mismatch-list.5
  (mismatch '(a b c) '(b c) :start1 1)
  nil)

(deftest mismatch-list.6
  (mismatch '(a b c d) '(z b c e) :start1 1 :start2 1)
  3)

(deftest mismatch-list.7
  (mismatch '(a b c d) '(z b c e) :start1 1 :start2 1 :end1 3 :end2 3)
  nil)

(deftest mismatch-list.8
  (mismatch '(1 2 3 4) '(5 6 7 8) :test #'(lambda (x y) (= x (- y 4))))
  nil)

(deftest mismatch-list.9
  (mismatch '(1 2 3 4) '(5 6 17 8) :test #'(lambda (x y) (= x (- y 4))))
  2)

(deftest mismatch-list.10
  (mismatch '(1 2 3 4) '(10 11 7 123) :test-not #'(lambda (x y) (= x (- y 4))))
  2)

(deftest mismatch-list.11
  (mismatch '(1 2 3 4) '(5 6 17 8) :key #'evenp)
  nil)

(deftest mismatch-list.12
  (mismatch '(1 2 3 4) '(5 6 12 8) :key 'oddp)
  2)

(deftest mismatch-list.13
  (mismatch '(1 2 3 4) '(1 2 3 4) :test 'eq)
  nil)

(deftest mismatch-list.14
  (mismatch '(1 2 3 4) '(5 6 7 8) :test-not 'eq)
  nil)

(deftest mismatch-list.15
  (mismatch '(a b c d e f g h i j k) '(a b c c e f g h z j k))
  3)

(deftest mismatch-list.16
  (mismatch '(a b c d e f g h i j k) '(a b c c y f g h z j k) :from-end t)
  9)

(deftest mismatch-list.17
  (mismatch '(a b c) '(a b c a b c d) :from-end t)
  3)

(deftest mismatch-list.18
  (mismatch '(a b c a b c d) '(a b c) :from-end t)
  7)

(deftest mismatch-list.19
  (mismatch '(1 1 1) '(2 2 2 2 2 1 2 2) :from-end t :test-not 'eq)
  1)

(deftest mismatch-list.20
  (mismatch '(1 1 1 1 1 1 1) '(2 3 3) :from-end t :key #'evenp)
  5)

(deftest mismatch-list.21
  (mismatch '(1 1 1) '(2 2 2 2 2 1 2 2) :from-end t :test-not #'equal)
  1)

(deftest mismatch-list.22
  (mismatch '(1 1 1 1 1 1 1) '(2 3 3) :from-end t :key 'evenp)
  5)

;;; tests on vectors

(deftest mismatch-vector.1
  (mismatch #() #(a b c))
  0)

(deftest mismatch-vector.2
  (mismatch #(a b c d) #())
  0)

(deftest mismatch-vector.3
  (mismatch #(a b c) #(a b c))
  nil)

(deftest mismatch-vector.4
  (mismatch #(a b c) #(a b d))
  2)

(deftest mismatch-vector.5
  (mismatch #(a b c) #(b c) :start1 1)
  nil)

(deftest mismatch-vector.6
  (mismatch #(a b c d) #(z b c e) :start1 1 :start2 1)
  3)

(deftest mismatch-vector.7
  (mismatch #(a b c d) #(z b c e) :start1 1 :start2 1 :end1 3 :end2 3)
  nil)

(deftest mismatch-vector.8
  (mismatch #(1 2 3 4) #(5 6 7 8) :test #'(lambda (x y) (= x (- y 4))))
  nil)

(deftest mismatch-vector.9
  (mismatch #(1 2 3 4) #(5 6 17 8) :test #'(lambda (x y) (= x (- y 4))))
  2)

(deftest mismatch-vector.10
  (mismatch #(1 2 3 4) #(10 11 7 123) :test-not #'(lambda (x y) (= x (- y 4))))
  2)

(deftest mismatch-vector.11
  (mismatch #(1 2 3 4) #(5 6 17 8) :key #'evenp)
  nil)

(deftest mismatch-vector.12
  (mismatch #(1 2 3 4) #(5 6 12 8) :key 'oddp)
  2)

(deftest mismatch-vector.13
  (mismatch #(1 2 3 4) #(1 2 3 4) :test 'eq)
  nil)

(deftest mismatch-vector.14
  (mismatch #(1 2 3 4) #(5 6 7 8) :test-not 'eq)
  nil)

(deftest mismatch-vector.15
  (mismatch #(a b c d e f g h i j k) #(a b c c e f g h z j k))
  3)

(deftest mismatch-vector.16
  (mismatch #(a b c d e f g h i j k) #(a b c c y f g h z j k) :from-end t)
  9)

(deftest mismatch-vector.17
  (mismatch #(a b c) #(a b c a b c d) :from-end t)
  3)

(deftest mismatch-vector.18
  (mismatch #(a b c a b c d) #(a b c) :from-end t)
  7)

(deftest mismatch-vector.19
  (mismatch #(1 1 1) #(2 2 2 2 2 1 2 2) :from-end t :test-not 'eq)
  1)

(deftest mismatch-vector.20
  (mismatch #(1 1 1 1 1 1 1) #(2 3 3) :from-end t :key #'evenp)
  5)

(deftest mismatch-vector.21
  (mismatch #(1 1 1) #(2 2 2 2 2 1 2 2) :from-end t :test-not #'equal)
  1)

(deftest mismatch-vector.22
  (mismatch #(1 1 1 1 1 1 1) #(2 3 3) :from-end t :key 'evenp)
  5)

;;; tests on bit vectors

(deftest mismatch-bitvector.1
  (mismatch "" #*111)
  0)

(deftest mismatch-bitvector.1a
  (mismatch '() #*111)
  0)

(deftest mismatch-bitvector.1b
  (mismatch "" '(1 1 1))
  0)

(deftest mismatch-bitvector.2
  (mismatch #*1010 #*)
  0)

(deftest mismatch-bitvector.2a
  (mismatch #*1010 '())
  0)

(deftest mismatch-bitvector.2b
  (mismatch '(1 0 1 0) #*)
  0)

(deftest mismatch-bitvector.3
  (mismatch #*101 #*101)
  nil)

(deftest mismatch-bitvector.4
  (mismatch #*101 #*100)
  2)

(deftest mismatch-bitvector.5
  (mismatch #*101  #*01 :start1 1)
  nil)

(deftest mismatch-bitvector.6
  (mismatch #*0110 #*0111 :start1 1 :start2 1)
  3)

(deftest mismatch-bitvector.7
  (mismatch #*0110 #*0111 :start1 1 :start2 1 :end1 3 :end2 3)
  nil)

(deftest mismatch-bitvector.7a
  (mismatch '(0 1 1 0) #*0111 :start1 1 :start2 1 :end1 3 :end2 3)
  nil)

(deftest mismatch-bitvector.7b
  (mismatch #*0110 '(0 1 1 1) :start1 1 :start2 1 :end1 3 :end2 3)
  nil)

(deftest mismatch-bitvector.8
  (mismatch #*1001 #*0110 :test #'(lambda (x y) (= x (- 1 y))))
  nil)

(deftest mismatch-bitvector.8a
  (mismatch #*1001 '(5 4 4 5) :test #'(lambda (x y) (= x (- y 4))))
  nil)

(deftest mismatch-bitvector.9
  (mismatch #*1001 '(5 4 17 5) :test #'(lambda (x y) (= x (- y 4))))
  2)

(deftest mismatch-bitvector.9a
  (mismatch '(5 4 17 5) #*1001 :test #'(lambda (x y) (= y (- x 4))))
  2)

(deftest mismatch-bitvector.9b
  (mismatch #*0100 #*1001 :test #'(lambda (x y) (= x (- 1 y))))
  2)

(deftest mismatch-bitvector.10
  (mismatch #*1001 '(10 11 4 123) :test-not #'(lambda (x y) (= x (- y 4))))
  2)

(deftest mismatch-bitvector.10a
  (mismatch #*1001 '(10 11 100 123) :test-not #'(lambda (x y) (= x (- y 4))))
  nil)

(deftest mismatch-bitvector.11
  (mismatch #*1010 '(5 6 17 8) :key #'evenp)
  nil)

(deftest mismatch-bitvector.11a
  (mismatch '(5 6 17 8) #*1010 :key #'evenp)
  nil)

(deftest mismatch-bitvector.11b
  (mismatch #*0101 #*1010 :key #'evenp :test-not 'eq)
  nil)

(deftest mismatch-bitvector.11c
  (mismatch '(5 6 17 8) #*10101 :key #'evenp)
  4)

(deftest mismatch-bitvector.11d
  (mismatch '(5 6 17 8 100) #*1010 :key #'evenp)
  4)

(deftest mismatch-bitvector.12
  (mismatch #*1010 #*1000 :key 'oddp)
  2)

(deftest mismatch-bitvector.12a
  (mismatch #*1010 '(5 6 8 8) :key 'oddp)
  2)

(deftest mismatch-bitvector.12b
  (mismatch '(5 6 8 8) #*1010 :key 'oddp)
  2)

(deftest mismatch-bitvector.13
  (mismatch #*0001 #*0001 :test 'eq)
  nil)

(deftest mismatch-bitvector.14
  (mismatch '#*10001 #*01110 :test-not 'eq)
  nil)

(deftest mismatch-bitvector.15
  (mismatch #*00100010100 #*00110010000)
  3)

(deftest mismatch-bitvector.16
  (mismatch #*00100010100 #*00110010000 :from-end t)
  9)

(deftest mismatch-bitvector.17
  (mismatch #*001 #*0010010 :from-end t)
  3)

(deftest mismatch-bitvector.18
  (mismatch #*0010010 #*001 :from-end t)
  7)

(deftest mismatch-bitvector.19
  (mismatch #*000 #*11111011 :from-end t :test-not 'eq)
  1)

(deftest mismatch-bitvector.20
  (mismatch #*1111111 '(2 3 3) :from-end t :key #'evenp)
  5)

(deftest mismatch-bitvector.21
  (mismatch #*111 #*00000100 :from-end t :test-not #'equal)
  1)

(deftest mismatch-bitvector.22
  (mismatch #*1111111 '(2 3 3) :from-end t :key 'evenp)
  5)

;;; tests on strings

(deftest mismatch-string.1
  (mismatch "" "111")
  0)

(deftest mismatch-string.1a
  (mismatch '() "111")
  0)

(deftest mismatch-string.1b
  (mismatch "" '(1 1 1))
  0)

(deftest mismatch-string.2
  (mismatch "1010" "")
  0)

(deftest mismatch-string.2a
  (mismatch "1010" '())
  0)

(deftest mismatch-string.2b
  (mismatch '(1 0 1 0) "")
  0)

(deftest mismatch-string.3
  (mismatch "101" "101")
  nil)

(deftest mismatch-string.4
  (mismatch "101" "100")
  2)

(deftest mismatch-string.5
  (mismatch "101" "01" :start1 1)
  nil)

(deftest mismatch-string.6
  (mismatch "0110" "0111" :start1 1 :start2 1)
  3)

(deftest mismatch-string.7
  (mismatch "0110" "0111" :start1 1 :start2 1 :end1 3 :end2 3)
  nil)

(deftest mismatch-string.7a
  (mismatch '(#\0 #\1 #\1 #\0) "0111" :start1 1 :start2 1 :end1 3 :end2 3)
  nil)

(deftest mismatch-string.7b
  (mismatch "0110" '(#\0 #\1 #\1 #\1) :start1 1 :start2 1 :end1 3 :end2 3)
  nil)

(deftest mismatch-string.8
  (mismatch "1001" "0110" :test #'(lambda (x y) (eql x (if (eq y #\0) #\1 #\0))))
  nil)

(deftest mismatch-string.8a
  (mismatch "1001" '(5 4 4 5) :test #'(lambda (x y)
					(setq x (read-from-string (string x)))
					(= x (- y 4))))
  nil)

(deftest mismatch-string.9
  (mismatch "1001" '(5 4 17 5) :test #'(lambda (x y)
					 (setq x (read-from-string (string x)))
					 (= x (- y 4))))
  2)

(deftest mismatch-string.9a
  (mismatch '(5 4 17 5) "1001" :test #'(lambda (x y)
					 (setq y (read-from-string (string y)))
					 (= y (- x 4))))
  2)

(deftest mismatch-string.9b
  (mismatch "0100" "1001" :test #'(lambda (x y) (eql x (if (eql y #\0) #\1 #\0))))
  2)

(deftest mismatch-string.10
  (mismatch "1001" "0049" :test-not #'(lambda (x y)
					 (setq x (read-from-string (string x)))
					 (setq y (read-from-string (string y)))
					 (eql x (- y 4))))
  2)

(deftest mismatch-string.10a
  (mismatch "1001" "3333" :test-not #'(lambda (x y)
					(setq x (read-from-string (string x)))
					(setq y (read-from-string (string y)))
					(eql x (- y 4))))
  nil)

(deftest mismatch-string.11
  (mismatch "1010" "5678"  :key #'evendigitp)
  nil)

(deftest mismatch-string.11a
  (mismatch "5678" "1010" :key #'odddigitp)
  nil)

(deftest mismatch-string.11b
  (mismatch "0101" "1010" :key #'evendigitp :test-not 'eq)
  nil)

(deftest mismatch-string.11c
  (mismatch "5678" "10101" :key #'evendigitp)
  4)

(deftest mismatch-string.11d
  (mismatch "56122" "1010" :key #'evendigitp)
  4)

(deftest mismatch-string.11e
  (mismatch "0101" '(#\1 #\0 #\1 #\0) :key #'evendigitp :test-not 'eq)
  nil)

(deftest mismatch-string.12
  (mismatch "1010" "1000" :key 'odddigitp)
  2)

(deftest mismatch-string.12a
  (mismatch "1010" "5688" :key 'odddigitp)
  2)

(deftest mismatch-string.12b
  (mismatch '(#\5 #\6 #\8 #\8) "1010" :key 'odddigitp)
  2)

(deftest mismatch-string.13
  (mismatch "0001" "0001" :test 'eq)
  nil)

(deftest mismatch-string.14
  (mismatch "10001" "01110" :test-not 'eq)
  nil)

(deftest mismatch-string.15
  (mismatch "00100010100" "00110010000")
  3)

(deftest mismatch-string.16
  (mismatch "00100010100" "00110010000" :from-end t)
  9)

(deftest mismatch-string.17
  (mismatch "001" "0010010" :from-end t)
  3)

(deftest mismatch-string.18
  (mismatch "0010010" "001" :from-end t)
  7)

(deftest mismatch-string.19
  (mismatch "000" "11111011" :from-end t :test-not 'eq)
  1)

(deftest mismatch-string.20
  (mismatch "1111111" "233" :from-end t :key #'evendigitp)
  5)

(deftest mismatch-string.20a
  (mismatch "1111111" '(#\2 #\3 #\3) :from-end t :key #'evendigitp)
  5)

(deftest mismatch-string.21
  (mismatch "111" "00000100" :from-end t :test-not #'equal)
  1)

(deftest mismatch-string.22
  (mismatch "1111111" "233" :from-end t :key 'evendigitp)
  5)
