;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Sun Sep 15 07:42:36 2002
;;;; Contains: Auxiliary functions for testing REMOVE and related functions

(in-package :cl-test)

(defun make-random-element (type)
  (cond
   ((subtypep 'fixnum type)
    (random most-positive-fixnum))
   ((subtypep '(integer 0 255) type)
    (random 255))
   ((subtypep '(integer 0 7) type)
    (random 8))
   ((subtypep 'bit type)
    (random 2))
   ((subtypep 'symbol type)
    (elt '(a b c d e f g h) (random 8)))
   ((subtypep '(member #\a #\b #\c #\d #\e #\f #\g #\h) type)
    (elt "abcdefgh" (random 8)))
   (t (error "Can't get random element of type ~A~%." type))))

(defun random-from-seq (seq)
  "Generate a random member of a sequence."
  (let ((len (length seq)))
    (assert (> len 0))
    (elt seq (random len))))

(defmacro random-case (&body cases)
  (let ((len (length cases)))
    (assert (> len 0))
    `(case (random ,len)
       ,@(loop for i from 0 for e in cases collect `(,i ,e))
       (t (error "Can't happen?! (in random-case~%")))))

(defun make-random-remove-input (len type element-type)

  "Randomly generate a test case for REMOVE.  Given a length
   a sequence type, and an element type, produce a random
   sequence of length LEN of sequence type TYPE, and either
   generate a random member of the sequence or a random
   element of the element type to delete from the sequence."
  
  (let* ((seq (if (subtypep type 'list)
		  (loop for i from 1 to len collect
			(make-random-element element-type))
		(let ((seq (make-sequence type len)))
		  (dotimes (i len)
		    (setf (elt seq i) (make-random-element element-type)))
		  seq)))
	 (e (if (and (> len 0) (eql (random 2) 0))
		(elt seq (random len))
	      (make-random-element element-type)))
	 )
    (values len seq e)))

(defun my-remove (element
		  sequence
		  &key
		  (start 0)
		  (end nil)
		  (test #'eql test-p)
		  (test-not nil test-not-p)
		  (key nil)
		  (from-end nil)
		  (count nil))
  (assert (not (and test-p test-not-p)))
  (my-remove-if
   (cond (test-p #'(lambda (x) (funcall test element x)))
	 (test-not-p #'(lambda (x) (not (funcall test-not element x))))
	 (t #'(lambda (x) (eql element x))))
   sequence :start start :end end :key key :from-end from-end :count count))
  
(defun my-remove-if (predicate
		     original-sequence
		     &key (from-end nil)
		     (start 0)
		     (end nil)
		     (count nil)
		     (key #'identity))
  (let ((len (length original-sequence))
	(sequence (copy-seq original-sequence)))
    (unless end (setq end len))
    (unless key (setq key #'identity))
    (unless count (setq count len))
    
    ;; Check that everything's kosher
    (assert (<= 0 start end len))
    (assert (typep sequence 'sequence))
    (assert (integerp count))
    (assert (or (symbolp predicate) (functionp predicate)))
    (assert (or (symbolp key) (functionp key)))
    
    ;; If FROM-END, reverse the sequence and flip
    ;; start, end
    (when from-end
      (psetq sequence (nreverse sequence)
	     start (- len end)
	     end (- len start)))

    ;; Accumulate a list of elements for the result
    (let ((pos 0)
	  (result nil)) ;; accumulate in reverse order
      (map nil
	   #'(lambda (e)
	       (if (and (> count 0)
			(>= pos start)
			(< pos end)
			(funcall predicate (funcall key e)))
		   (decf count)
		 (push e result))
	       (incf pos))
	   sequence)
      (unless from-end
	(setq result (nreverse result)))
      ;; Convert to the correct type
      (if (listp sequence)
	  result
	(let ((element-type (array-element-type original-sequence)))
	  (make-array (length result) :element-type element-type
		      :initial-contents result))))))

(defun my-remove-if-not (pred &rest args)
  (apply #'my-remove-if (complement pred) args))

(defun coin (&optional (n 2)) (eql (random n) 0))

;; Randomly permute a sequence
(defun random-permute (seq)
  (setq seq (copy-seq seq))
  (let ((len (length seq)))
    (loop for i from len downto 2
	  do (let ((r (random i)))
	       (rotatef (elt seq r) (elt seq (1- i))))))
  seq)

(defun make-random-rd-params (maxlen)
  "Generate random paramaters for remove/delete/etc. functions."
  (let* ((element-type t)
	 (type-select (random 7))
	 (type
	  (case type-select
	    (0 'list)
	    (1 'vector)
	    (2 (setq element-type 'character) 'string)
	    (3 (setq element-type 'bit) 'bit-vector)
	    (4 'simple-vector)
	    (5 (setq element-type '(integer 0 255))
	       '(vector (integer 0 255)))
	    (6 (setq element-type 'fixnum) '(vector fixnum))
	    (t (error "Can't happen?!~%"))))
	 (len (random maxlen))
	 (start (and (coin) (> len 0)
		     (random len)))
	 (end (and (coin)
		   (if start (+ start (random (- len start)))
		     (random (1+ len)))))
	 (from-end (coin))
	 (count (and (coin)
		     (random (1+ len))))
	 (seq (multiple-value-bind (x y z) (make-random-remove-input len type element-type)
		y))
	 (key (and (coin)
		   (case type-select
		     (2 (random-case
			 #'char-upcase 'char-upcase
			 #'char-downcase 'char-downcase))
		     (3 #'(lambda (x) (- 1 x)))
		     ((5 6) (random-case #'1+ '1+ #'1- '1-))
		     (t (random-case 'identity #'identity)))))
	 (test (and (eql (random 3) 0)
		    (random-case 'eq 'eql 'equal
				 #'eq #'eql #'equal)))
	 (test-not (and (not test)
			(coin)
			(random-case 'eq 'eql 'equal
				     #'eq #'eql #'equal)))
	 )
    ;; Return parameters
    (values
     element-type type len start end from-end count seq key test test-not)))

(defun random-test-remove-args (maxlen)
  (multiple-value-bind (element-type type len start end from-end count seq key test test-not)
      (make-random-rd-params maxlen)
    (let ((element (if (and (coin) (> len 0))
		       (random-from-seq seq)
		     (make-random-element element-type)))
	  (arg-list
	   (reduce #'nconc
		   (random-permute
		    (list
		     (when start (list :start start))
		     (cond (end (list :end end))
			   ((coin) (list :end nil)))
		     (cond (from-end (list :from-end from-end))
			   ((coin) (list :from-end nil)))
		     (cond (count (list :count count))
			   ((coin) (list :count nil)))
		     (cond (key (list :key key))
			   ;; ((coin) (list :key nil))
			   )
		     (when test (list :test test))
		     (when test-not (list :test test-not)))))))
      (values element seq arg-list))))

(defun random-test-remove (maxlen)
  (multiple-value-bind (element seq arg-list)
      (random-test-remove-args maxlen)
    (let* ((seq1 (copy-seq seq))
	   (seq2 (copy-seq seq))
	   (seq1r (apply #'remove element seq1 arg-list))
	   (seq2r (apply #'my-remove element seq1 arg-list)))
      (cond
       ((not (equalp seq seq1)) :fail1)
       ((not (equalp seq seq2)) :fail2)
       ((not (equalp seq1r seq2r)) :fail3)
       (t t)))))
