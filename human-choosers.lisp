;;;; human-choosers.lisp
;;;; Code for generating choosers that use humans to make choices.

;;;; todo Write a variant human chooser which augments a human player
;;;; by providing them with remembered information, such as other
;;;; player's hands when they are known.

(in-package :robotonosushigo)

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

