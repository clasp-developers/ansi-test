;-*- Mode:     Lisp -*-
;;;; Author:   Tarn W. Burton
;;;; Created:  Sun Apr 4 15:00:00 2025
;;;; Contains: Tests of the ~$ format directive





(deftest format.$.4
  (let ((fn (formatter "~,,4$")))
    (loop for x in (remove-duplicates '(1 1.0s0 1.0f0 1.0d0 1.0l0))
          for s = (format nil "~,,4$" x)
          for s2 = (formatter-call-to-string fn x)
          unless (and (string= s "1.00") (string= s s2))
          collect (list x s s2)))
  nil)

(deftest format.$.5
  (let ((fn (formatter "~,,3$")))
    (loop for x in (remove-duplicates '(1 1.0s0 1.0f0 1.0d0 1.0l0))
          for s = (format nil "~,,3$" x)
          for s2 = (formatter-call-to-string fn x)
          unless (and (string= s "1.00") (string= s s2))
          collect (list x s s2)))
  nil)

(deftest format.$.6
  (let ((fn (formatter "~,,5$")))
    (loop for x in (remove-duplicates '(1 1.0s0 1.0f0 1.0d0 1.0l0))
          for s = (format nil "~,,5$" x)
          for s2 = (formatter-call-to-string fn x)
          unless (and (string= s " 1.00") (string= s s2))
          collect (list x s s2)))
  nil)

(deftest format.$.7
  (let ((fn (formatter "~,,5@$")))
    (loop for x in (remove-duplicates '(1 1.0s0 1.0f0 1.0d0 1.0l0))
          for s = (format nil "~,,5@$" x)
          for s2 = (formatter-call-to-string fn x)
          unless (and (string= s "+1.00") (string= s s2))
          collect (list x s s2)))
  nil)

(deftest format.$.8
  (let ((fn (formatter "~,,4@$")))
    (loop for x in (remove-duplicates '(1 1.0s0 1.0f0 1.0d0 1.0l0))
          for s = (format nil "~,,4@$" x)
          for s2 = (formatter-call-to-string fn x)
          unless (and (string= s "+1.00") (string= s s2))
          collect (list x s s2)))
  nil)

(deftest format.$.9
  (let ((fn (formatter "~,,5$")))
    (loop for x in (remove-duplicates '(1 1.0s0 1.0f0 1.0d0 1.0l0))
          for s = (format nil "~,,5$" (- x))
          for s2 = (formatter-call-to-string fn (- x))
          unless (and (string= s "-1.00") (string= s s2))
          collect (list (- x) s s2)))
  nil)

(deftest format.$.10
  (let ((fn (formatter "~,,4$")))
    (loop for x in (remove-duplicates '(1/2 0.5s0 0.5f0 0.5d0 0.5l0))
          for s = (format nil "~,,4$" x)
          for s2 = (formatter-call-to-string fn x)
          unless (and (string= s "0.50") (string= s s2))
          collect (list x s s2)))
  nil)

(deftest format.$.11
  (let ((fn (formatter "~,,5$")))
    (loop for x in (remove-duplicates '(1/2 0.5s0 0.5f0 0.5d0 0.5l0))
          for s = (format nil "~,,5$" x)
          for s2 = (formatter-call-to-string fn x)
          unless (and (string= s " 0.50") (string= s s2))
          collect (list x s s2)))
  nil)

(deftest format.$.12
  (let ((fn (formatter "~3,,5$")))
    (loop for x in (remove-duplicates '(1/2 0.5s0 0.5f0 0.5d0 0.5l0))
          for s = (format nil "~3,,5$" x)
          for s2 = (formatter-call-to-string fn x)
          unless (and (string= s "0.500") (string= s s2))
          collect (list x s s2)))
  nil)

(deftest format.$.13
  (let ((fn (formatter "~3,0,4$")))
    (loop for x in (remove-duplicates '(1/2 0.5s0 0.5f0 0.5d0 0.5l0))
          for s = (format nil "~3,0,4$" x)
          for s2 = (formatter-call-to-string fn x)
          unless (and (string= s ".500") (string= s s2))
          collect (list x s s2)))
  nil)

(deftest format.$.14
  (let ((fn (formatter "~1,0,2$")))
    (loop for x in (remove-duplicates '(1/2 0.5s0 0.5f0 0.5d0 0.5l0))
          for s = (format nil "~1,0,2$" x)
          for s2 = (formatter-call-to-string fn x)
          unless (and (string= s ".5") (string= s s2))
          collect (list x s s2)))
  nil)

(deftest format.$.15
  (let ((fn (formatter "~2,0,4@$")))
    (loop for x in (remove-duplicates '(1/2 0.5s0 0.5f0 0.5d0 0.5l0))
          for s = (format nil "~2,0,4@$" x)
          for s2 = (formatter-call-to-string fn x)
          unless (and (string= s "+.50") (string= s s2))
          collect (list x s s2)))
  nil)

(deftest format.$.16
  (let ((fn (formatter "~2,0,2$")))
    (loop for x in (remove-duplicates '(1/2 0.5s0 0.5f0 0.5d0 0.5l0))
          for s = (format nil "~2,0,2$" x)
          for s2 = (formatter-call-to-string fn x)
          unless (and (string= s ".50") (string= s s2))
          collect (list x s s2)))
  nil)

(deftest format.$.17
  (let ((fn (formatter "~3$")))
    (loop for x in (remove-duplicates '(1/2 0.5s0 0.5f0 0.5d0 0.5l0))
          for s = (format nil "~3$" x)
          for s2 = (formatter-call-to-string fn x)
          unless (and (string= s "0.500") (string= s s2))
          collect (list x s s2)))
  nil)

(deftest format.$.18
  (let ((fn (formatter "~3$")))
    (loop for xn in (remove-duplicates '(1/2 0.5s0 0.5f0 0.5d0 0.5l0))
          for x = (- xn)
          for s = (format nil "~3$" x)
          for s2 = (formatter-call-to-string fn x)
          unless (and (string= s "-0.500") (string= s s2))
          collect (list x s s2)))
  nil)

;;; padchar

(deftest format.$.26
  (let ((fn (formatter "~1,,10$")))
    (loop for x in (remove-duplicates
                    '(100 100.0s0 100.0f0 100.0d0 100.0l0))
          for s = (format nil "~1,,10$" x)
          for s2 = (formatter-call-to-string fn x)
          unless (and (string= s "     100.0") (string= s s2))
          collect (list x s s2)))
  nil)

(deftest format.$.27
  (let ((fn (formatter "~1,,10,'*$")))
    (loop for x in (remove-duplicates
                    '(100 100.0s0 100.0f0 100.0d0 100.0l0))
          for s = (format nil "~1,,10,'*$" x)
          for s2 = (formatter-call-to-string fn x)
          unless (and (string= s "*****100.0") (string= s s2))
          collect (list x s s2)))
  nil)

;;; v parameters

(deftest format.$.28
  (let ((fn (formatter "~v$")))
    (loop for x = (random 100.0)
          for s1 = (format nil "~$" x)
          for s2 = (format nil "~v$" nil x)
          for s3 = (formatter-call-to-string fn nil x)
          repeat 100
          unless (and (string= s1 s2) (string= s2 s3))
          collect (list x s1 s2 s3)))
  nil)

(deftest format.$.29
  (let ((fn (formatter "~,v$")))
    (loop for x = (random 100.0)
          for s1 = (format nil "~$" x)
          for s2 = (format nil "~,v$" nil x)
          for s3 = (formatter-call-to-string fn nil x)
          repeat 100
          unless (and (string= s1 s2) (string= s2 s3))
          collect (list x s1 s2 s3)))
  nil)

(deftest format.$.30
  (let ((fn (formatter "~,,v$")))
    (loop for x = (random 100.0)
          for s1 = (format nil "~$" x)
          for s2 = (format nil "~,,v$" nil x)
          for s3 = (formatter-call-to-string fn nil x)
          repeat 100
          unless (and (string= s1 s2) (string= s2 s3))
          collect (list x s1 s2 s3)))
  nil)

(deftest format.$.31
  (let ((fn (formatter "~,,,v$")))
    (loop for x = (random 100.0)
          for s1 = (format nil "~$" x)
          for s2 = (format nil "~,,,v$" nil x)
          for s3 = (formatter-call-to-string fn nil x)
          repeat 100
          unless (and (string= s1 s2) (string= s2 s3))
          collect (list x s1 s2 s3)))
  nil)

;;; Randomized tests

#|
(deftest format.$.33
  (let ((bound (if (> 10000000 most-positive-short-float)
                   most-positive-short-float
                 (coerce 10000000 'short-float))))
    (loop for d = (random 10)
          for w = (+ 1 d (random 10))
          for x = (random bound)
          for xr = (rational x)
          for s = (format nil "~v,vf" w d x)
          for sr = (decode-fixed-decimal-string s)
          for eps = (expt 1/10 d)
          for abs-xr-sr = (abs (- xr sr))
          for abs-xr-sr-hi = (abs (- xr (+ sr eps)))
          for abs-xr-sr-lo = (abs (- xr (- sr eps)))
          repeat 100
          unless (and (<= abs-xr-sr abs-xr-sr-hi)
                      (<= abs-xr-sr abs-xr-sr-lo))
          collect (list d w x xr s sr eps abs-xr-sr abs-xr-sr-hi abs-xr-sr-lo)))
  nil)
|#

(deftest format.$.34
  (with-standard-io-syntax
   (let ((*read-default-float-format* 'short-float))
     (loop for i from (- 1 (ash 1 13)) below (ash 1 13)
           for sf = (coerce i 'short-float)
           for s = (format nil "~$" sf)
           for i2 = (floor (read-from-string s))
           unless (or (zerop i) (eql i i2))
           collect (list i sf s i2))))
  nil)

(deftest format.$.35
  (with-standard-io-syntax
   (let ((*read-default-float-format* 'single-float))
     (loop for i = (- (random (1- (ash 1 25))) -1 (ash 1 24))
           for sf = (coerce i 'single-float)
           for s = (format nil "~$" sf)
           for i2 = (floor (read-from-string s))
           repeat 2000
           unless (or (zerop i) (eql i i2))
           collect (list i sf s i2))))
  nil)

(deftest format.$.36
  (with-standard-io-syntax
   (let ((*read-default-float-format* 'double-float))
     (loop for i = (- (random (1- (ash 1 51))) -1 (ash 1 50))
           for sf = (coerce i 'double-float)
           for s = (format nil "~$" sf)
           for i2 = (floor (read-from-string s))
           repeat 2000
           unless (or (zerop i) (eql i i2))
           collect (list i sf s i2))))
  nil)

(deftest format.$.37
  (with-standard-io-syntax
   (let ((*read-default-float-format* 'long-float))
     (loop for i = (- (random (1- (ash 1 51))) -1 (ash 1 50))
           for sf = (coerce i 'long-float)
           for s = (format nil "~$" sf)
           for i2 = (floor (read-from-string s))
           repeat 2000
           unless (or (zerop i) (eql i i2))
           collect (list i sf s i2))))
  nil)

(deftest format.$.38
  (funcall
   (compile
    nil
    '(lambda ()
       (with-standard-io-syntax
        (let ((*read-default-float-format* 'short-float)
              (total 0)
              (len 0))
          (loop for i from (- 1 (ash 1 13)) below (ash 1 13)
                unless (zerop i)
                nconc
                (loop for sf = (coerce i 'short-float)
                      for w = (random 8)
                      for d = (random 4)
                      for s = (format nil "~v,,v$" d w sf)
                      for i2 = (ignore-errors (floor (read-from-string s)))
                      repeat 5
                      ; do (print (list w d s i i2))
                      unless (eql i i2)
                      do (incf total)
                      and collect (list i sf w d s i2))
                when (> total 100) collect "count limit exceeded"
                and do (loop-finish)))))))
  nil)

(deftest format.$.39
  (with-standard-io-syntax
   (let ((*read-default-float-format* 'single-float))
     (loop for i = (- (random (1- (ash 1 25))) -1 (ash 1 24))
           for sf = (coerce i 'single-float)
           for w = (and (coin) (random 16))
           for d = (random 4)
           for s = (format nil "~v,,v$" d w sf)
           for i2 = (floor (read-from-string s))
           repeat 2000
           unless (or (zerop i) (eql i i2))
           collect (list i sf w d s i2))))
  nil)

(deftest format.$.40
  (with-standard-io-syntax
   (let ((*read-default-float-format* 'double-float))
     (loop for i = (- (random (1- (ash 1 51))) -1 (ash 1 50))
           for sf = (coerce i 'double-float)
           for w = (and (coin) (random 30))
           for d = (random 6)
           for s = (format nil "~v,,v$" d w sf)
           for i2 = (floor (read-from-string s))
           repeat 2000
           unless (or (zerop i) (eql i i2))
           collect (list i sf w d s i2))))
  nil)

(deftest format.$.41
  (with-standard-io-syntax
   (let ((*read-default-float-format* 'long-float))
     (loop for i = (- (random (1- (ash 1 51))) -1 (ash 1 50))
           for sf = (coerce i 'long-float)
           for w = (and (coin) (random 30))
           for d = (random 6)
           for s = (format nil "~v,,v$" d w sf)
           for i2 = (floor (read-from-string s))
           repeat 2000
           unless (or (zerop i) (eql i i2))
           collect (list i sf w d s i2))))
  nil)

(deftest format.$.42
  (let ((chars +standard-chars+))
    (loop
     for x = (random (random-from-seq #(#.(coerce (* 32 (1- (ash 1 13))) 'short-float)
                                        #.(coerce (* 256 (1- (ash 1 24))) 'single-float)
                                        #.(coerce (* 256 (1- (ash 1 50))) 'double-float)
                                        #.(coerce (* 256 (1- (ash 1 50))) 'long-float))))
     for w = (and (coin) (random 30))
     for d = (and (coin) (random 10))
     for n = (and (coin) (random 10))
     for padchar = (and (coin) (random-from-seq chars))
     for f1 = (concatenate 'string
                           "~"
                           (if d (format nil "~d" d) "")
                           ","
                           (if n (format nil "~d" n) "")
                           ","
                           (if w (format nil "~d" w) "")
                           ","
                           (if padchar (format nil "'~c" padchar) "")
                           "$")
     for s1 = (format nil f1 x)
     for s2 = (format nil "~v,v,v,v$" d n w padchar x)
     repeat 2000
     unless (string= s1 s2)
     collect (list x d n w padchar f1 s1 s2)))
  nil)

(def-format-test format.$.43
  "~,,,',$" (0.0) "0.00")

(deftest format.$.44
  (loop for i from 0 below (min #x10000 char-code-limit)
        for x = 2312.9817
        for c = (code-char i)
        for f1 = (and c (format nil "~~,,,'~c$" c))
        for s1 = (and c (ignore-errors (format nil f1 x)))
        for s2 = (and c (format nil "~,,,v$" c x))
        unless (equal s1 s2)
        collect (list i c f1 s1 s2))
  nil)
