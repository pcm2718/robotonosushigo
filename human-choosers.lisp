;;;; human-choosers.lisp
;;;; Code for generating choosers that use humans to make choices.

;;;; todo Write a variant human chooser which augments a human player
;;;; by providing them with remembered information, such as other
;;;; player's hands when they are known.

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

;; todo Can I find a method that uses return as a delimiter or
;; something? read-delimited-list unsafe?
(defun read-choice ()
  (read-delimited-list #\Period))

;; todo Ugly. Name? Prompt?
(defun read-choice-until-valid (board hand)
  (loop initially (format t "Cards: ")
        for choice = (read-choice) then (read-choice)
        if (valid-choice-p choice board hand)
          return choice
        do (format t "Again: ")
        ;; todo Does the newline go here?
        finally (format t "~%")))

;; While I could pass in other information through the arguments, it
;; seems to me that there is no reason to do so for any information
;; that does not change between the start of the game and the
;; conclusion of the game. Things like the number of players and their
;; names may as well be compiled in to a generated human-chooser
;; function rather than have those same arguments passed in by the
;; model to a single human-chooser function just for the sake of
;; avoiding run-time compilation.
;; todo Evaling a quoted lambda expression feels ugly, I'm sure
;; there's a better way to do this.
(defun make-human-chooser (pos names)
  (eval
   `(lambda (pudding boards hand)
      (let ((pboard (nth ,pos boards)))
        (loop for b in boards
              for p in pudding
              for n in ',names
              for i below ,(length names)
              unless (eq ,pos i)
                do (format t "~A has played: ~A and ~
                              has ~A pudding~[s~;~:;s~] from previous rounds.~%"
                           (nth i ',names) b p p))
        ;; Find a way to insert name directly into the format string.
        (format t "You (~A) have played: ~A and ~
                   have ~A pudding~[s~;~:;s~] from previous rounds.~%~
                   Your hand is: ~A~%"
                ;; todo Elimintate redundancy.
                ,(nth pos names) pboard (nth ,pos pudding) (nth ,pos pudding) hand)
        (read-choice-until-valid pboard hand)))))

