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

(defvar *cvc5-configuration*
  (make-instance 'smt:solver*
                 :program "cvc5"
                 :arguments (list "--lang" "sygus"
                                  "--produce-models"
                                  "--incremental"
                                  )))

(defmethod smt-solver-configuration ((solver (eql :sygus)) &key &allow-other-keys)
  *cvc5-configuration*)

(defun %check-specification (specification)
  "Checks that SPECIFICATION is valid for a SyGuS problem"
  (and (spec:is-pbe? specification)
       (every #'(lambda (a b) (eql (spec:descriptor a) (spec:descriptor b)))
              (spec:examples specification)
              (rest (spec:examples specification)))))

(defun %chc-output-sort (chc)
  "Gets the inferred output sort of CHC"
  (let ((os (chc:output-symbols chc)))
    (and (= 1 (length os))
         (chc:symbol-sort (elt os 0)))))

(defun %check-chc (chc context)
  "Checks that CHC is valid SyGuS semantics"
  (and
   ;; Check that children only take overall inputs
   (loop for child in (chc:body chc)
         unless (every #'(lambda (ia) (input-var? ia chc))
                       (chc:input-actuals child))
           do (return nil)
         finally (return t))
   ;; Eventually support tuple outputs...but not yet
   (= 1 (length (chc:output-symbols chc)))
   ;; Make sure term type output sorts match
   (let ((tt (chc:term-type (chc:head chc)))
         (ios (%chc-output-sort chc)))
     (a:if-let (prev-output-sort (tt-sort tt context))
       (eql ios prev-output-sort)   ; Must be the same inferred sort
       (setf (tt-sort tt context) ios))) ; Otherwise set this as the first
   ;; Check the bodies for approved content
   (let ((os (chc:symbol-name (elt (chc:output-symbols chc) 0))))
     (?:match (chc:constraint chc)
       ((?:guard (or (smt:fn "=" ((smt:var o) cbody))
                     (smt:fn "=" (cbody (smt:var o))))
                 (eql o os))
        (let ((info (convert-chc-constraint cbody chc nil)))
          (if info
              (setf (chc:get-data :sygus-prod chc) info)
              nil)
          info))
       (_ nil)))))

(defmethod transform-specification ((solver (eql :sygus)) specification context)
  "Transforms the SemGuS specification SPECIFICATION to a SyGuS specification"
  ;; Check 1: the spec is only a standard PBE specification (to expand later)
  (let ((res (and (%check-specification specification)
                  (every (a:rcurry #'%check-chc context) (semgus:chcs context))
                  specification)))
    (when res
      (let ((synth-fun (make-synth-fun context)))
        (setf (synth-fun context) synth-fun)
        (setf res (convert-constraints specification context))
        res))))

(defmethod initialize-solver ((solver (eql :sygus))
                              &key &allow-other-keys)
  "We should convert the problem here eventually or something. Or check it."
  nil)

(defun resurrect-program (expr context)
  "Resurrects a SemGuS program from a SyGuS function definition"
  (?:match expr
    ((smt:var name)
     (loop for chc in (semgus:chcs context)
           for syprod = (chc:get-data :sygus-prod chc)
           when (and (typep syprod (find-class 'input-production))
                     (eql (variable syprod) name))
             do (return (make-instance 'ast:program-node
                                       :production (g:lookup-production
                                                    context
                                                    (chc:name
                                                     (chc:constructor chc)))))
           finally (error "Unable to match input variable")))
    ((smt:fn name children)
     (let ((chc (find name (semgus:chcs context)
                      :key (a:compose
                            #'(lambda (x)
                                (if (typep x (find-class 'function-production))
                                    (smt:ensure-identifier (name x))
                                    nil))
                            (a:curry #'chc:get-data :sygus-prod))))
           (production nil))
       (loop for prod in (g:productions context)
             when (eql (g:name (g:operator prod)) (chc:name (chc:constructor chc)))
               do (progn (setf production prod) (return))
             finally (error "Unable to match a production"))
       (make-instance 'ast:program-node
                      :production production
                      :children (map 'list
                                     (a:rcurry #'resurrect-program context)
                                     children))))))

(defmethod solve-problem ((solver (eql :sygus)) semgus-problem &key &allow-other-keys)
  "Dump the problem as SMT commands to the SMT solver"
  (let ((context (semgus:context semgus-problem)))
    (smt:with-solver* (s (smt-solver-configuration solver))
      (smt:set-logic s "ALL")

      ;; CHC function definitions
      (loop for chc in (semgus:chcs context)
            for decl = (smt-declaration (chc:get-data :sygus-prod chc) context)
            when decl
              do (smt:dump s (smt:to-smt decl :pprint t)))

      ;; Synth-fun
      (smt:dump s (smt:to-smt (synth-fun context) :pprint t))

      ;; Constraints
      (smt:dump s (smt:to-smt (semgus:specification semgus-problem) :pprint t))

      ;; Do it!
      (smt:dump s (format nil "(check-synth)~%"))

      ;; Get the model?
      (a:when-let* ((model (smt:read-model s))
                    (definition (smt:definition (first model)))
                    (result (resurrect-program definition context)))
        (format t "~&SyGuS solver returned: ~a~%" result)
        result))))

(defun test-solve (path)
  (let ((problem (semgus:load-semgus-problem path)))
    (initialize-solver :sygus)
    (let ((spec (transform-specification :sygus
                                         (semgus:specification problem)
                                         (semgus:context problem))))
      (if spec
          (let ((new-problem (semgus:replace-specification problem spec)))
            (solve-problem :sygus new-problem))
          nil))))
