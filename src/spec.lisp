;;;;
;;;; Specification handling
;;;;
(in-package #:systems.duck.ks2.sygus)

(defclass sygus-specification ()
  ((constraints :reader constraints
                :initarg :constraints
                :type list ; of constraints that SyGuS recognizes...
                :documentation "List of SyGuS constraints")))

(defclass pbe-constraint ()
  ((synth-fun :reader synth-fun
              :initarg :synth-fun
              :type synth-fun
              :documentation "The function")
   (output :reader output
           :initarg :output
           :type smt::expression
           :documentation "The output value")
   (inputs :reader inputs
           :initarg :inputs
           :type smt:state
           :documentation "The input state")))

(defmethod spec:leaf-specification-types ((spec sygus-specification))
  (list 'pbe-constraint))

(defun convert-pbe-constraint (pbe context)
  "Converts a PBE constraint to a SyGuS-style constraint"
  (make-instance 'pbe-constraint
                 :synth-fun (synth-fun context)
                 :output (smt:get-first-value (spec:output-state pbe))
                 :inputs (spec:input-state pbe)))

(defmethod smt:to-smt ((pbe pbe-constraint) &key pprint)
  (declare (ignore pprint))
  (format nil "(constraint (= ~a ~a))"
          (smt:to-smt (apply #'smt:$apply
                             (smt-fn (synth-fun pbe))
                             (loop for f in (formals (synth-fun pbe))
                                   collect
                                   (smt:$literal
                                    (smt:get-value (inputs pbe) (smt:name f))
                                    (smt:sort f))))
                      :pprint t)
          (smt:to-smt (smt:$literal (output pbe)
                                    (sort (synth-fun pbe)))
                      :pprint t)))

(defun convert-constraints (specification context)
  "Converts all constraints in SPECIFICATION"
  (make-instance 'sygus-specification
                 :constraints (map 'list
                                   (a:rcurry #'convert-pbe-constraint context)
                                   (spec:examples specification))))

(defmethod smt:to-smt ((spec sygus-specification) &key pprint)
  (declare (ignore pprint))
  (with-output-to-string (ss)
    (loop for constraint in (constraints spec)
          do (format ss "~a~%" (smt:to-smt constraint :pprint t)))))
