;;;;
;;;; Data storage encapsulation
;;;;
(in-package #:systems.duck.ks2.sygus)

(defun tt-sort (term-type context &optional default)
  "Gets the derived output sort of TERM-TYPE from CONTEXT"
  (a:if-let (tt-map (gethash :sygus-tt-map (semgus:metadata context)))
    (gethash term-type tt-map)
    default))

(defun (setf tt-sort) (sort term-type context)
  "Sets the derived output sort of TERM-TYPE to SORT in CONTEXT"
  (let ((tt-map (gethash :sygus-tt-map (semgus:metadata context))))
    (unless tt-map
      (setf tt-map (make-hash-table))
      (setf (gethash :sygus-tt-map (semgus:metadata context)) tt-map))
    (setf (gethash term-type tt-map) sort)))

;; Note: declared as a method because sygus-pbe has it as a reader method
(defmethod synth-fun ((context semgus:semgus-context))
  "Gets the synth-fun from CONTEXT"
  (gethash :sygus-synth-fun (semgus:metadata context)))

(defmethod (setf synth-fun) (sf (context semgus:semgus-context))
  "Sets the synth-fun in CONTEXT"
  (setf (gethash :sygus-synth-fun (semgus:metadata context)) sf))
