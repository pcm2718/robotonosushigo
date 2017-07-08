;;;; robotonosushigo.asd
;;;; robotonosushigo ASDF package definition, or:
;;;; ロボトのすしごー、 です。
;;;; todo Is the non-ASCII joke above a bad idea? Even if it isn't, I
;;;; should improve the translation. Add testing code in a separate
;;;; package?

(defpackage :robotonosushigo (:use :asdf :cl))
(in-package :robotonosushigo)

(defsystem robotonosushigo
  :name "robotonosushigo"
  :author "Parker Michaelson"
  :version "0.1"
  :maintainer "Parker Michaelson"
  ;; todo Change description to match start of README.md?
  :description "A computerized version of the card game Sushi Go."
  ;; tod Change description to match README.md? Add link to Github
  ;; repo?
  :long-description ""
  :components
  ((:file "decks")
   (:file "scoring")
   (:file "model" :depends-on ("scoring"))
   (:file "human-choosers")
   (:file "random-chooser" :depends-on ("human-choosers"))))
