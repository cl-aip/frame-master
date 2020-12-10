(eval-when (:compile-toplevel :load-toplevel :execute)
  (require :asdf)
  )
(defpackage :xrl-system
  (:use :common-lisp :asdf))

(in-package :xrl-system)

(defsystem :xrl
  :name "xrl"
  :author "Eugene Charniak, Chistopher Riesbeck, Drew V. McDermott, and James R. Meehan"
  :maintainer "Seiji Koide <koide@ontolonomy.co.jp>"
  :version "0.0.1"
  :description "Prototype of simple frame system"
  :long-description "presented in 'Artificial Intelligence Programming', 2nd Ed., LEA (1987)"
  :components
  ((:file "xrl")))