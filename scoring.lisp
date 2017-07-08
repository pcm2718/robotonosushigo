;;;; scoring.lisp
;;;; Logic for assorted scoring tasks. These are all functional in
;;;; nature and don't muck with any closures passed in from above.

;;;; Note regarding terminology: The term "board" is borrowed from
;;;; poker, here I use it to mean, at any given point in a round, the
;;;; cards so far played by a specific player. The sense of "board"
;;;; as, at any given point in a round, the cards played by *all* of
;;;; the players so far is not used here for clarity.

(in-package :robotonosushigo)

;; todo Look into using nlet or lambda let or something or replacing
;; this entirely with symbol properties lists. Maybe a closured macro
;; of some sort?
(defun score-nigiri (x)
  (case x
    (eg 1)
    (sl 2)
    (sq 3)
    (wa 0)))

;; Takes a player's board, ordered from most to least recently played,
;; and returns a list containing the player's individual score (the
;; player's score for the round before scoring maki), the player's
;; maki count, and the player's pudding count.
(defun score-board (board)
  (let ((ddelta 1)
        (nigiri ())
        (maki 0)
        (sashimi 0)
        (tempura 0)
        (score 0)
        (pudding 0))
    (loop for cards in board
          ;; Special case for simultaneously played nigiri and
          ;; wasabi. Ensures that wasabi the nigiri stack before the
          ;; nigiri.
          if (and (member 'wa cards)
                  (or (member 'eg cards)
                      (member 'sl cards)
                      (member 'sq cards)))
            do 
               (push (car (remove 'wa cards)) nigiri)
               (push 'wa nigiri)
          else 
            do (loop for card in cards
                     ;; todo See if there's a way to replace this with a system
                     ;; that calls functions (possibly in a closure) bound to the
                     ;; card symbols. Properties list?
                     do (case card
                          ;; If you're confused by this, notice the deltas on the
                          ;; dumpling score table.
                          ;; todo Can I figure out a way to trampoline
                          ;; this statement out after five dumplings,
                          ;; and is it worth it? Is this efficient?
                          (du (when (< ddelta 6)
                                (incf score ddelta)
                                (incf ddelta)))
                          ;; The nigiri and wasabi is handled in a separate step
                          ;; after this loop.
                          ;; todo Using four separate statements like
                          ;; this is ugly, see if there's a way to
                          ;; access the evaluation of keyform.
                          (eg (push 'eg nigiri))
                          (sl (push 'sl nigiri))
                          (sq (push 'sq nigiri))
                          (wa (push 'wa nigiri))
                          ;; todo I still think separating the maki into separate
                          ;; symbolic representations was the right move. I'll see
                          ;; if I still think that later.
                          (m1 (incf maki))
                          (m2 (incf maki 2))
                          (m3 (incf maki 3))
                          ;; Pudding
                          (pd (incf pudding))
                          ;; We could score the sashimi by triplets and tempura by
                          ;; pairs as we go, but I'm confident that it's faster to
                          ;; accumulate all the sashimi and tempura and use floor
                          ;; division to compute the score instead of branching
                          ;; every time we get here.
                          ;; todo If I wanted to be super-efficient, I
                          ;; think I could incf by 10 and 5 here, then
                          ;; round down or something later instead of
                          ;; flooring and multiplying. Kind of
                          ;; sketchy, though.
                          (sh (incf sashimi))
                          (tm (incf tempura)))))
    ;; Score the nigiri and wasabi.
    ;; todo Questionably efficient.
    (loop
      for x in nigiri
      with wasabi = 0
      do (if (eq 'wa x)
             (incf wasabi)
             (if (zerop wasabi)
                 (incf score (score-nigiri x))
                 (progn
                   (incf score (* 3 (score-nigiri x)))
                   (decf wasabi)))))
    ;; Score the sashimi and tempura.
    (incf score (* 10 (floor sashimi 3)))
    ;; Score the tempura.
    (incf score (* 5 (floor tempura 2)))
    (list score maki pudding)))

;; Takes a list of boards and returns a list containing two lists with
;; elements ordered corresponding to boards: a list of board scores
;; and a list of board pudding counts.
(defun score-boards (boards)
  (let* ((raws (mapcar #'score-board boards))
         (scores (mapcar #'car raws))
         (maki6maki 0)
         (maki6players ())
         (maki3maki 0)
         (maki3players ()))
    ;; Find which players have the most maki and which players have
    ;; the second-most maki.
    (loop
      for maki in (mapcar #'cadr raws)
      for pos by 1
      do (when (not (zerop maki))
           (cond ((< maki6maki maki)
                  (setf maki3maki maki6maki
                        maki3players maki6players
                        maki6maki maki
                        maki6players (list pos)))
                 ((= maki6maki maki)
                  (setf maki6players (cons pos maki6players)))
                 ((< maki3maki maki)
                  (setf maki3maki maki
                        maki3players (list pos)))
                 ((= maki3maki maki)
                  (setf maki3players (cons pos maki3players))))))
    ;; todo I think I could minimize the branches here with a little
    ;; more thought.
    ;; Award points to the players with the most maki.
    (loop
      for pos in maki6players
      and delta = (floor 6 (length maki6players))
      do (incf (nth pos scores) delta))
    ;; Award points to the players with the second-most maki if there
    ;; was only one player with the most maki.
    (unless (cdr maki6players)
      (loop
        for pos in maki3players
        and delta = (floor 3 (length maki3players))
        do (incf (nth pos scores) delta)))
    ;; Return a list containing a list of maki-corrected scores and a
    ;; list of puddings counts, both in the order corresponding to the
    ;; input boards.
    (list scores (mapcar #'caddr raws))))

;; Takes a list of pudding counts and returns a corresponding list of
;; scores.
;; todo Add two player behavior where no points are deducted from the
;; player with less pudding. Improve description. How do I handle the
;; pudding based tie breaking?
(defun score-pudding (pudding)
  ;; todo This method works but is slow, I just want something working
  ;; now, though, so I'll patch this up with the superior old code
  ;; below later.
  (let ((maxpud (apply #'max pudding))
        (minpuds '())
        (minpud (apply #'min pudding))
        (maxpuds '())
        (scores (make-list (length pudding) :initial-element 0)))
    (unless (= minpud maxpud)
      (loop
        for pud in pudding
        for pos from 0
        do (cond
             ((= minpud pud) (setf minpuds (cons pos minpuds)))
             ((= maxpud pud) (setf maxpuds (cons pos maxpuds)))))
      (loop
        for pud in maxpuds
        with maxpudbonus = (floor 6 (length maxpuds))
        do (incf (nth pud scores) maxpudbonus))
      (if (< 2 (length pudding))
          (loop
            for pud in minpuds
            with minpudbonus = (floor 6 (length minpuds))
            do (decf (nth pud scores) minpudbonus))))
    scores)
#||
  ;; Score the pudding.
  (let ((first-player (car players))
        (least-pudding (list (player-pudding first-player) first-player))
        (most-pudding (list (player-pudding first-player) first-player)))
    (dolist (p (cdr players))
      (let ((lp (car least-pudding))
            (mp (car most-pudding))
            (pp (player-pudding p)))
        ;; todo When lp and mp are different, one and only one test
        ;; will succeed. When lp and mp are the same, at most two
        ;; tests will succeed. Optimize this by skipping to the next
        ;; iteration when appropriate, by putting the replacers first?
        (if (< pp lp) (setf least-pudding (list pp p)))
        (if (= pp lp) (setf least-pudding (cons least-pudding p)))
        (if (= pp mp) (setf most-pudding (cons most-pudding p)))
        (if (> pp mp) (setf most-pudding (list pp p)))))
    ;; todo Add six to p's score.
    (dolist (p (cdr most-pudding)))
    ;; todo Drop six from p's score.
    (dolist (p (cdr least-pudding))))
||#)
