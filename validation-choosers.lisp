;;;; validation-chooser.lisp
;;;; Code for generating validation choosers.

;;;; A validation chooser is a chooser which is a wrapper around
;;;; another chooser. It feeds the standard chooser arguments to the
;;;; other chooser and validates the other chooser's choice as a legal
;;;; move before returing it. Its intended application is as a wrapper
;;;; for untrustworthy choosers, preventing them from silently making
;;;; illegal choices.

;;;; todo Move validation code to a separate file? Write some
;;;; automated testing code for both the chooser and the validation
;;;; code itself.

(in-package :robotonosushigo)

;; todo Because boards are generated from the log, which tracks
;; chopsticks that have been played at some point, and not necessarily
;; chopsticks that are currently in the any board, this function is
;; necessary to make ensure that chopsticks remain balanced with
;; usage. I really need to find a better solution than this at some
;; point.
(defun count-chopsticks (board)
  (reduce #'+ (mapcar #'(lambda (x) (count 'ch x)) board)))

(defun valid-chopstick-use-p (board)
  (< (count-if #'(lambda (x) (eq 2 (length x))) board)
     (count-chopsticks board)))

;; todo More descriptive name. Fix comment style. Currently validates
;; playing two cards when chopsticks have already been used. todo Fix hacky
;; sexp that ensures chopsticks are balanced with length two lists.
(defun valid-choice-p (choice board hand)
  ;; The first (and possibly only) card chosen by the player must be
  ;; in the player's hand.
  (and (member (car choice) hand)
       ;; There must also be only one card chosen, or
       (or (eq 1 (length choice))
           ;; only two cards chosen, in which case
           (and (eq 2 (length choice))
                ;; the second card must be in the player's hand and
                (member (cadr choice) hand)
                ;; chopsticks must be in the player's board.
                ;;(member 'ch board)
                ;; the hacky chopstick balancer must return true.
                (valid-chopstick-use-p board)
                ;; todo Ugly, rewrite comments to put the eq statement down here.
                t))))

;; todo Should this be a macro? What happens if I pass in a lambda as
;; the chooser?
(defun make-validation-chooser (chooser)
  (eval
   `(lambda (pudding board hand)
      (let ((choice (funcall ,chooser pudding board hand)))
        (if (valid-choice-p choice board hand)
            choice
            ;; If the choice made by chooser is *not* valid, signal an
            ;; error.
            ;; todo Use the condition system to signal the error
            ;; properly and allow the operator to inspect and modify
            ;; choice. Compile the chooser name directly into the
            ;; string. Improve error message. Is the printed form of
            ;; chooser wrong when chooser is a named function? More
            ;; printed information without debugger?
            (error "~A has returned an invalid choice, ~A." ',chooser choice))))))
