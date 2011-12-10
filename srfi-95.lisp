;;;; srfi-95.lisp

(cl:in-package :srfi-95.internal)

;;; "sort.scm" Defines: sorted?, merge, merge!, sort, sort!
;;; Author : Richard A. O'Keefe (based on Prolog code by D.H.D.Warren)
;;;
;;; This code is in the public domain.

;;; Updated: 11 June 1991
;;; Modified for scheme library: Aubrey Jaffer 19 Sept. 1991
;;; Updated: 19 June 1995
;;; (sort, sort!, sorted?): Generalized to strings by jaffer: 2003-09-09
;;; (sort, sort!, sorted?): Generalized to arrays by jaffer: 2003-10-04
;;; jaffer: 2006-10-08:
;;; (sort, sort!, sorted?, merge, merge!): Added optional KEY argument.
;;; jaffer: 2006-11-05:
;;; (sorted?, merge, merge!, sort, sort!): Call KEY arg at most once
;;; per element.
;;; jaffer: 2007-01-29: Final SRFI-95.
;;; CHIBA Masaomi: 2011-12-10: Ported to Common Lisp

;;; (sorted? sequence less?)
;;; is true when sequence is a list (x0 x1 ... xm) or a vector #(x0 ... xm)
;;; such that for all 1 <= i <= m,
;;;	(not (less? (list-ref list i) (list-ref list (- i 1)))).
;@
(defun sorted? (seq less? &optional (opt-key #'identity) &aux (key opt-key))
  (cond ((null seq) t)
        ((arrayp seq)
         (let ((dimax (+ -1 (car (array-dimensions seq)))))
           (or (<= dimax 1)
               (let loop ((idx (+ -1 dimax))
                          (last (funcall key (aref seq dimax))) )
                    (or (minusp idx)
                        (let ((nxt (funcall key (aref seq idx))))
                          (and (funcall less? nxt last)
                               (loop (+ -1 idx) nxt) )))))))
        ((null (cdr seq)) t)
        (:else
         (let loop ((last (funcall key (car seq)))
                    (next (cdr seq)) )
              (or (null next)
                  (let ((nxt (funcall key (car next))))
                    (and (not (funcall less? nxt last))
                         (loop nxt (cdr next)) )))))))

;;; (merge a b less?)
;;; takes two lists a and b such that (sorted? a less?) and (sorted? b less?)
;;; and returns a new list in which the elements of a and b have been stably
;;; interleaved so that (sorted? (merge a b less?) less?).
;;; Note:  this does _not_ accept arrays.  See below.
;@
(defun merge (a b less? &optional (opt-key #'identity) &aux (key opt-key))
  (cond ((null a) b)
	((null b) a)
	(:else
	 (let loop ((x (car a)) (kx (funcall key (car a))) (a (cdr a))
		    (y (car b)) (ky (funcall key (car b))) (b (cdr b)))
	   ;; The loop handles the merging of non-empty lists.  It has
	   ;; been written this way to save testing and car/cdring.
	   (if (funcall less? ky kx)
	       (if (null b)
		   (cons y (cons x a))
		   (cons y (loop x kx a (car b) (funcall key (car b)) (cdr b))))
	       ;; x <= y
	       (if (null a)
		   (cons x (cons y b))
		   (cons x (loop (car a) (funcall key (car a)) (cdr a) y ky b))))))))

(defun |SORT:MERGE!| (a b less? key)
  (labels ((loop (r a kcara b kcarb)
                 (cond ((funcall less? kcarb kcara)
                        (setf (cdr r) b)
                        (if (null (cdr b))
                            (setf (cdr b) a)
                            (loop b a kcara (cdr b) (funcall key (cadr b))) ))
                       (:else				; (car a) <= (car b)
                        (setf (cdr r) a)
                        (if (null (cdr a))
                            (setf (cdr a) b)
                            (loop a (cdr a) (funcall key (cadr a)) b kcarb) )))))
    (cond ((null a) b)
          ((null b) a)
          (:else
           (let ((kcara (funcall key (car a)))
                 (kcarb (funcall key (car b))) )
             (cond
               ((funcall less? kcarb kcara)
                (if (null (cdr b))
                    (setf (cdr b) a)
                    (loop b a kcara (cdr b) (funcall key (cadr b))) )
                b )
               (:else			; (car a) <= (car b)
                (if (null (cdr a))
                    (setf (cdr a) b)
                    (loop a (cdr a) (funcall key (cadr a)) b kcarb) )
                a )))))))

;;; takes two sorted lists a and b and smashes their cdr fields to form a
;;; single sorted list including the elements of both.
;;; Note:  this does _not_ accept arrays.
;@
(defun merge! (a b less? &optional (opt-key #'identity))
  (|SORT:MERGE!| a b less? opt-key))

(defun |SORT:SORT-LIST!| (seq less? &optional key)
  (flet ((keyer (x &aux (key (if key #'car #'identity)))
           (funcall key x)))
    (labels ((step (n)
               (cond ((> n 2) (let* ((j (floor n 2))
                                     (a (step j))
                                     (k (- n j))
                                     (b (step k)))
                                (|SORT:MERGE!| a b less? #'keyer)))
                     ((= n 2) (let ((x (car seq))
                                    (y (cadr seq))
                                    (p seq))
                                (setq seq (cddr seq))
                                (cond ((funcall less? (keyer y) (keyer x))
                                       (setf (car p) y)
                                       (setf (car (cdr p)) x)))
                                (setf (cdr (cdr p)) '())
                                p))
                     ((= n 1) (let ((p seq))
                                (setq seq (cdr seq))
                                (setf (cdr p) '())
                                p))
                     (:else '())))
             (key-wrap! (lst)
               (cond ((null lst))
                     (:else (setf (car lst) (cons (funcall key (car lst))
                                                  (car lst)))
                            (key-wrap! (cdr lst)))))
             (key-unwrap! (lst)
               (cond ((null lst))
                     (:else (setf (car lst) (cdar lst))
                            (key-unwrap! (cdr lst))))))
      (cond (key
             (key-wrap! seq)
             (setq seq (step (length seq)))
             (key-unwrap! seq)
             seq)
            (:else
             (step (length seq)))))))

(defun rank-1-array->list (array
                           &aux (dimensions (array-dimensions array)))

  (do ((idx (+ -1 (car dimensions)) (+ -1 idx))
       (lst '() (cons (aref array idx) lst)))
      ((< idx 0) lst)))

;;; (sort! sequence less?)
;;; sorts the list, array, or string sequence destructively.  It uses
;;; a version of merge-sort invented, to the best of my knowledge, by
;;; David H. D. Warren, and first used in the DEC-10 Prolog system.
;;; R. A. O'Keefe adapted it to work destructively in Scheme.
;;; A. Jaffer modified to always return the original list.
;@
(defun sort! (seq less? &optional opt-key)
  (cond ((arrayp seq)
	 (do ((sorted (|SORT:SORT-LIST!| (rank-1-array->list seq) less? opt-key)
		      (cdr sorted))
	      (i 0 (+ i 1)))
	     ((null sorted) seq)
	   (setf (aref seq (car sorted)) i)))
	(:else			      ; otherwise, assume it is a list
	 (let ((ret (|SORT:SORT-LIST!| seq less? opt-key)))
	   (if (not (eq ret seq))
	       (do ((crt ret (cdr crt)))
		   ((eq (cdr crt) seq)
		    (setf (cdr crt) ret)
		    (let ((scar (car seq)) (scdr (cdr seq)))
		      (setf (car seq) (car ret)) (setf (cdr seq) (cdr ret))
		      (setf (car ret) scar) (setf (cdr ret) scdr)))))
	   seq))))

;;; (sort sequence less?)
;;; sorts a array, string, or list non-destructively.  It does this
;;; by sorting a copy of the sequence.  My understanding is that the
;;; Standard says that the result of append is always "newly
;;; allocated" except for sharing structure with "the last argument",
;;; so (append x '()) ought to be a standard way of copying a list x.
;@
(defun sort (seq less? &optional opt-key)
  (cond ((arrayp seq)
	 (let ((newra (copy-seq seq)))
	   (do ((sorted (|SORT:SORT-LIST!| (rank-1-array->list seq) less? opt-key)
			(cdr sorted))
		(i 0 (+ i 1)))
	       ((null sorted) newra)
	     (setf (aref newra i) (car sorted)))))
	(:else (|SORT:SORT-LIST!| (append seq '()) less? opt-key))))

;;; eof