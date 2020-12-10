;;;-*- Mode: common-lisp; syntax: common-lisp; package: xrl; base: 10 -*-
;;;
;;;; A Frame System, xrl
;;;
;;;   from Chapter 13 of Artificial Intelligence Programming, 2nd. 
;;;   by Eugene Charniack, Christopher K. Riesbeck, 
;;;   Drew V. McDermott, and James R. Meehan.
;;;   LEA, 1987.
;;;
;;; To work around the name collegion on cg:form in ACL,
;;; symbol `form' is changed to frame.
;;; To work around the name collegion on slot-value in Common Lisp,
;;; symbol `slot-value' is changed to fget.
;;; To make it easy to read the program, symbol `aspect' is changed to
;;; facet.
;;;
;;; Even though avoiding the name collegions mentioned above, 
;;; all definitions are in a dedicated package, xrl.

(defpackage xrl
  (:use :cl)
  (:documentation "This package is newly settled for xrl frame system."))

(in-package :xrl)

;;;
;;; Figure 13.4 Storing frames in Hash Tables
;;; 
(defvar *slot-table-size* 10
  "expected average number of slots")
(defvar *facet-table-size* 3
  "expected average number of facets")

;;; There are three kinds of hash tables:
;;;  *frame-table* has {<frame-name>, <frame>} entries.
;;;    Each frame has a slot-table.
;;;  A slot-table has {<slot-name>, <facet-table>} entries.
;;;    But <slot-name> is simply called <slot> in the following code.
;;;  A facet-table has {<facet>, <value>} entries.
;;;    <facet> is a symbol like =, if-needed, if-added, etc.

(defvar *frame-table* (make-hash-table))

;;; A frame has three parts: a name, an isa, and a 
;;;   table of slots. To reduce printout, a frame
;;;   prints as #<frame: name>.

(defstruct (frame
            (:print-function
             (lambda (frame stream depth)
               (declare (ignore depth))
               (format stream "#<frame: ~S>"
                 (frame-name frame)))))
  name
  (isa nil)
  (slot-table
   (make-hash-table :size *slot-table-size*)))

(defun get-frame (frame-name)
  "return the frame stored under frame-name,
   if any.  If none, signal an error."
  (multiple-value-bind (frame found-p)
      (gethash frame-name *frame-table*)
    (if found-p
        frame
      (error "No such frame as ~S.~%" frame-name))))

(defun frame-of (frame-name)
  (gethash frame-name *frame-table*))

(defun set-frame (frame-name)
  "return the frame stored under <frame-name>,
   if any.  If none, make one."
  (multiple-value-bind (frame found-p)
      (gethash frame-name *frame-table*)
    (if found-p
        frame
      (setf (gethash frame-name *frame-table*)
        (make-frame :name frame-name)))))

;;;
;;; Figure 13.5 Storing Slots and facets in Hash Tables
;;;

(defun get-facet (frame slot facet)
  "return the value of <facet> of <slot> of <frame>."
  (assert (typep frame 'frame) ()
          "The first argument frame must be an instance of frame.")
  (let ((slot-table (frame-slot-table frame)))
    ;; the slot-table is always defined when a frame is made.
    (let ((facet-table (gethash slot slot-table)))
      (when (hash-table-p facet-table)
        ;; returns only a primary value
        (values (gethash facet facet-table))))))

(defun set-facet (frame slot facet value)
  "set the value of <facet> of <slot> in <frame>."
  (let ((slot-table (frame-slot-table frame)))
    (setf (gethash facet
                   (force-entry slot slot-table
                                #'(lambda ()
                                    (make-hash-table
                                     :size *facet-table-size*))))
      value)))

(defun force-entry (index table maker)
  (multiple-value-bind (entry found-p)
      (gethash index table)
    (if found-p
        entry
      (setf (gethash index table)
        (funcall maker)))))

(defun explicit-frame-slots (frame)
  "returns a list of the slot names stored with <frame>."
  (let ((slots '()))
    (maphash #'(lambda (slot facet-table)
                 (declare (ignore facet-table))
                 (push slot slots))
             (frame-slot-table frame))
    slots))

;;;
;;; Figure 13.6 Frame Definition Macro
;;;

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defmacro frame (&key (name (gensym "frame")) isa with)
    "(frame :name name
            :isa super-name
            :with ((<slot> <facet> <value>)*))
     Macro that expands into a call to <store-frame>,
     generating a value for <name>, if missing,
     and quoting everything except the <value>s."
    `(store-frame ',name ',isa
                 (list ,@(loop for spec in with
                             collect `(list ',(first spec)
                                            ',(second spec)
                                            ,(third spec))))))
  
  (defun store-frame (frame-name isa with)
    "return a frame whose name is frame-name, with an 
     isa link to the frame named by super-name, if 
     given, and whose slots and facets are set by 
     spec-list."
    (let ((frame (set-frame frame-name)))
      (when isa (setf (frame-isa frame) (set-frame isa)))
      (loop for spec in with
          do (apply #'set-facet frame spec))
      frame))
  )

;;;
;;; Figure 13.7 Initial Code for Creating and Using Frames
;;;

(defun fget (frame slot)
  (flet ((if-needed-val (an-isa slot frame)
                        (let ((fn (get-facet an-isa slot
                                              :if-needed)))
                          (if fn (funcall fn frame) nil))))
    (some-isa
     #'(lambda (an-isa)
         (or (get-facet an-isa slot '=)
             (if-needed-val an-isa slot frame)))
     frame)))

(defun some-isa (fn frame)
  (cond ((null frame) nil)
        ((funcall fn frame))
        (t (some-isa fn (frame-isa frame)))))

(defun set-fget (frame slot value)
  (set-facet frame slot '= value))
(defsetf fget set-fget)

(defun frame-slots (frame)
  (let ((slots '()))
    (some-isa
     #'(lambda (an-isa)
         (setq slots
               (union slots
                      (explicit-frame-slots an-isa)))
         nil)
     frame)
    slots))

#|
cg-user(1): (in-package :xrl)
#<The xrl package>
xrl(2): (frame :name thing)
#<frame: thing>
xrl(3): (frame :name dwelling :isa thing)
#<frame: dwelling>
xrl(4): (frame :name apartment :isa dwelling)
#<frame: apartment>
xrl(5): (frame :name apt-at-100-york
               :isa apartment
               :with ((street-name = 'york)
                      (street-number = 100)
                      (wall-color = 'white)
                      (floor-suface = 'wood)))
#<frame: apt-at-100-york>
xrl(6): (frame :name apt1
               :isa apt-at-100-york
               :with ((number-of-rooms = 3)))
#<frame: apt1>
xrl(7): (fget (get-frame 'apt1) 'wall-color)
white
xrl(8): (fget (get-frame 'apt1) 'number-of-rooms)
3
xrl(9): (defun get-street-noise (frame)
          (cond ((not (eq (fget frame 'street-name) 'york))
                 nil)
                ((> (fget frame 'street-number) 200)
                 'high)
                (t 'very-high)))
get-street-noise
xrl(10): (frame :name dwelling
                :isa thing
                :with ((street-noise
                        :if-needed #'get-street-noise)))
#<frame: dwelling>
xrl(11): (fget (get-frame 'apt1) 'street-noise)
very-high
xrl(12): (explicit-frame-slots (get-frame 'apt1))
(number-of-rooms)
xrl(13): (explicit-frame-slots (get-frame 'apt-at-100-york))
(street-name street-number floor-suface wall-color)
|#
