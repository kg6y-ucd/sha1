;; -*- coding: utf-8; -*-

;; sha1.lisp
;;
;; Copyright (c) 2015 Uchida Yasuo
;;
;; This software is released under the MIT License.
;; http://opensource.org/licenses/mit-license.php

(defpackage :sha1
  (:use :cl)
  (:export :sha1))

(in-package :sha1)

#|
Usage:

CL-USER> (sha1:sha1 "a")
"86F7E437FAA5A7FCE15D1DDCB9EAEAEA377667B8"
CL-USER> 
|#

(defun take (n lst &optional acc)
  (if (or (zerop n) (null lst))
      (reverse acc)
      (take (1- n) (cdr lst) (cons (car lst) acc))))

(defun drop (n lst)
  (nthcdr n lst))

(defun group (n lst &optional acc)
  (if (null lst)
      (reverse acc)
      (group n (drop n lst) (cons (take n lst) acc))))

(defun fix32 (w)
  (mod w (ash 1 32)))

(defun shift (n w)
  (fix32 (logior (ash w n)
		 (ash w (- n 32)))))

(defun from-list (lst)
  (make-array (length lst) :initial-contents lst))

(defun int-to-bits (n int)
  (loop for i from (1- n) downto 0 collect (if (logbitp i int) #b1 #b0)))

(defun bits-to-int (bits)
  (let ((w 0))
    (dolist (b bits w)
      (setf w (ash w 1))
      (unless (zerop b)
	(setf w (logior w #b1))))))

(defun octets-to-bits (octets)
  (reduce #'append octets
	  :from-end t
	  :key #'(lambda (o) (int-to-bits 8 o))))

(defun padding (bits)
  (let* ((l (length bits))
	 (l-bits (int-to-bits 64 l))
	 (zeros (make-list (- 512 (mod (+ l 1 64) 512)) :initial-element #b0)))
    (append bits (list #b1) zeros l-bits)))

(defun f (tt b c d)
  (fix32 (cond ((<= 0 tt 19) (logior (logand b c) (logand (lognot b) d)))
	       ((<= 20 tt 39) (logxor b c d))
	       ((<= 40 tt 59) (logior (logand b c) (logand b d) (logand c d)))
	       ((<= 60 tt 79) (logxor b c d)))))

(defun compute-digest (bits)
  (let ((k (from-list (append (loop repeat 20 collect #x5a827999)
			      (loop repeat 20 collect #x6ed9eba1)
			      (loop repeat 20 collect #x8f1bbcdc)
			      (loop repeat 20 collect #xca62c1d6))))
	(h0 #x67452301)
	(h1 #xefcdab89)
	(h2 #x98badcfe)
	(h3 #x10325476)
	(h4 #xc3d2e1f0)
	(m-lst (group 512 (padding bits))))
    (dolist (m m-lst)
      (let ((w (from-list (append (mapcar #'bits-to-int (group 32 m)) (make-list 64)))))
	(loop for tt from 16 to 79 do
	     (setf (aref w tt) (shift 1 (logxor (aref w (- tt 3))
						(aref w (- tt 8))
						(aref w (- tt 14))
						(aref w (- tt 16))))))
	(let ((a h0)
	      (b h1)
	      (c h2)
	      (d h3)
	      (e h4))
	  (loop for tt from 0 to 79 do
	       (setf temp (fix32 (+ (shift 5 a) (f tt b c d) e (aref w tt) (aref k tt)))
		     e d
		     d c
		     c (shift 30 b)
		     b a
		     a temp))
	  (setf h0 (fix32 (+ h0 a))
		h1 (fix32 (+ h1 b))
		h2 (fix32 (+ h2 c))
		h3 (fix32 (+ h3 d))
		h4 (fix32 (+ h4 e))))))
    (format nil "~{~8,'0x~}" (list h0 h1 h2 h3 h4))))

#+CCL
(defun sha1 (str)
  (compute-digest (octets-to-bits (ccl:encode-string-to-octets str :external-format :default))))
