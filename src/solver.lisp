;;;;
;;;; Solver configuration
;;;;
(in-package #:systems.duck.ks2.sygus)

(register-solver :sygus)

(define-solver-metadata :sygus
  :name "SyGuS Solver"
  :symbol "sygus"
  :description "An adapter for standard SyGuS solvers"
  :action "SyGuS Solve"
  :spec-transformer nil ; We'll probably want something eventually
  :options nil)

(defmethod initialize-solver ((solver (eql :sygus))
                              &key &allow-other-keys)
  "We should convert the problem here eventually or something. Or check it."
  nil)

(defmethod solve-problem ((solver (eql :sygus)) semgus-problem &key &allow-other-keys)
  "We should solve the problem here eventually..."
  nil)
