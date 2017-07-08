;;;; random-chooser.lisp
;;;; A chooser that chooses uniformly at random.

(in-package :robotonosushigo)

;; Return an element chosen uniformly at random from the list l.
(defun randomth (l)
  (nth (random (length l)) l))

;; random-chooser, given a board and hand, returns a valid, playable
;; card or pair of cards from hand as a list. The card or pair of
;; cards is chosen at uniformly at random from the list of every
;; possible choice. The list of every possible choice is constructed
;; as though every card in the hand is unique, even cards of the same
;; type, such that a hand containing, for example, two tempuras and
;; one egg nigiri will result in a list of all possible choices
;; containing "3 choose 2" = 3 choices.
;; todo Revise documentation for clarity.
(defun random-chooser (pudding board hand)
  (randomth (append (mapcar #'list hand)
                    ;; todo Find proper solution to the chopstick
                    ;; problem, so I can replace this with a simple
                    ;; call to member.
                    (when (valid-chopstick-use-p board)
                      (apply #'concatenate 'list
                             (loop for x on hand
                                   collect (loop for y in (cdr x)
                                                 collect (list (car x) y))))))))
