;;;; decks.lisp
;;;; Concerns the generation of randomized Sushi-go decks.
;;;; The deck is justified in being a closure, if it was not it would
;;;; simply be passed, returned, and setf'ed every other call,
;;;; traversing the stack its entire life.

;;;; Card notation key:
;;;; Chopsticks:          ch
;;;; Dumpling:            du
;;;; Egg Nigiri:          eg
;;;; 1 Maki roll:         m1
;;;; 2 Maki rolls:        m2
;;;; 3 Maki rolls:        m3
;;;; Pudding:             pd
;;;; Salmon Nigiri:       sl
;;;; Sashimi:             sh
;;;; Squid Nigiri:        sq
;;;; Tempura:             tm
;;;; Wasabi:              wa

;;;; Standard deck:
;;;; ch:   4
;;;; du:  14
;;;; eg:   5
;;;; m1:   6
;;;; m2:  12
;;;; m3:   8
;;;; pd:  10
;;;; sl:  10
;;;; sh:  14
;;;; sq:   5
;;;; tm:  14
;;;; wa:   6

(in-package :robotonosushigo)

;; todo Add safeties. I might be able to simplify this. len arg
;; questionable Does not delete the element in a one-element list
;; properly. Restructure as macro? Use pop?
(defun deleteth (n lis &optional len)
  (unless len
    (setf len (length lis)))
  (cond
    ;; todo Ugly hack!!!
    ;;((= len 1) (setf lis ()))
    ((= 0 n) (setf (car lis) (cadr lis)
                   (cdr lis) (cddr lis)))
    ;;((= (1- len) n) (setf (cdr (nthcdr (1- n) lis)) ()))
    (t (setf (cdr (nthcdr (1- n) lis)) (nthcdr (1+ n) lis))))
  lis)

;; todo I have worries about the results of this procedure. wa seems
;; disproportionately rare the first 20 elements.
(defun generate-deck ()
  ;; todo countx can't be quoted because the function modifies the
  ;; constant data because apparently destructive operations on quoted
  ;; data are undefined.
  (let* ((countx '((ch . 4)
                   (du . 14)
                   (eg . 5)
                   (m1 . 6)
                   (m2 . 12)
                   (m3 . 8)
                   (pd . 10)
                   (sl . 10)
                   (sh . 14)
                   (sq . 5)
                   (tm . 14)
                   (wa . 6)))
         (counts (copy-tree countx))
         (countssize 12)
         (deck ()))
    ;; todo Restructure to do-while?
    (do* ((r (random countssize) (random countssize))
          (x (nth r counts) (nth r counts)))
         ((= 1 countssize))
      (setf deck (cons (car x) deck))
      (if (< 1 (cdr x))
          (decf (cdr x))
          (progn
            (deleteth r counts countssize)
            (decf countssize))))
    ;; delete-nth doesn't delete the element in one-element lists
    ;; properly, so we have to handle the final element
    ;; ourselves.
    ;;todo Optimize this not to recompute cadr and caar every time.
    (dotimes (x (cdar counts) deck)
      (setf deck (cons (caar counts) deck)))))
