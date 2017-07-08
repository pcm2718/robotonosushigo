;;;; model.lisp
;;;; Sushi-go game logic.
;;;; todo Considering rewriting this in a way that is centered around
;;;; a central log structure that tracks the game history, allowing
;;;; for all visible information and history to be passed to an AI
;;;; player. Because the smallest discrete step, a round (?), would be
;;;; executed by calling a function with the log or state, it would
;;;; also be trivial to quit, resume and log games, and implement
;;;; branching for the convenience of AI implementation. Consider
;;;; leaving the pudding as cards in boards until the end of the game.

(in-package :robotonosushigo)

;; Takes a deck and the player count, returns the deck post-dealing
;; and the newly delt hands, the number and size of which are
;; determined by the player count as per Sushi-go's rules.
;; todo Ugly.
(defun deal-hands (deck playercount)
  (rotate-left
   (list
    (transpose (loop repeat (- 12 playercount)
                     collect (loop repeat playercount
                                   collect (pop deck)))) deck)))

;; Takes a list of players as well as correspondingly ordered lists of
;; pudding from previous rounds, boards, and hands. Returns a list of
;; cards chosen by the players ordered corresponding to the list of
;; players.
(defun get-player-choices (players pudding boards hands)
  (mapcar #'(lambda (p h) (funcall p pudding boards h)) players hands))

;; Courtesy of https://stackoverflow.com/a/3513158
(defun transpose (ll)
  (apply #'mapcar #'list ll))

;; Returns a list which is the list l rotated left by one element
;; without modfying l itself.
(defun rotate-left (l)
  (append (cdr l) (list (car l))))

(defun play-cards (cards hand board)
  (loop for card in cards
        do (setf hand (remove card hand :count 1))
        finally (push cards board)
                (when (cdr cards)
                  ;; todo Fix hacky chopstick balancing by removing
                  ;; played chopsticks from the hand here? I'd have to
                  ;; mess with rlog as well, wouldn't I?
                  (push 'ch hand)))
  (list hand board))

;; todo Should pudding really be an argument independent from log?
(defun play-round (deck pudding log players)
  (let* ((playercount (length players)))
    (destructuring-bind (deck hands) (deal-hands deck playercount)
      (cons deck
            ;; Subrounds
            (loop for sr below (- 11 playercount)
                  with boards = (make-list playercount :initial-element '())
                  ;; Get each player's choice of cards.
                  collect (get-player-choices players pudding boards hands) into rlog
                  do (destructuring-bind
                         (newhands newboards)
                         (transpose
                          ;; Modify each hand and board with the
                          ;; corresponding player's choice.
                          (mapcar #'play-cards (car (last rlog)) hands boards))
                       (setf hands (rotate-left newhands)
                             boards newboards))
                     ;; Once the hands are down to one card in size there is
                     ;; only one choice, so we don't need to ask the players for
                     ;; a choice.
                  finally (setf rlog (append rlog (list hands)))
                          (loop for board in boards
                                and choice in hands
                                do (push choice board))
                          (return (cons (append log (list rlog))
                                        (score-boards boards))))))))

;; todo I think I can crunch players down to just a list of functions
;; because I can compile names into the choosers that interface with
;; humans.
(defun play-game (deck players)
  (let ((log ())
        (scores (make-list (length players) :initial-element 0))
        (pudding (make-list (length players) :initial-element 0)))
    ;; Play the three rounds.
    ;; Ugly, should just do this all in one pass instead of two
    ;; mapcars. I could avoid the constant setfing by storing the
    ;; lists and mapcaring them all together at the end.
    (loop
       for round from 1 to 3
       do (destructuring-bind (rd rl rs rp) (play-round deck pudding log players)
            (setf deck rd
                  log rl
                  ;; todo mapcar #'+ macro?
                  scores (mapcar #'+ scores rs)
                  pudding (mapcar #'+ pudding rp))))
    ;; Until I find clear rules about tied scores and pudding, the
    ;; result of play-game will be a list of scores with no
    ;; tie-breaking done.
    (list deck log (mapcar #'+ scores (score-pudding pudding)))))
