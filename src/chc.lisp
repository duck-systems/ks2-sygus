;;;;
;;;; CHC manipulations
;;;;
(in-package #:systems.duck.ks2.sygus)

(defun input-var? (var chc)
  "Checks if VAR is an input to CHC"
  (find var (chc:input-symbols chc) :key #'chc:symbol-name))

(defun output-var? (var chc)
  "Checks if VAR is an output to CHC"
  (find var (chc:output-symbols chc) :key #'chc:symbol-name))

(defun %sort-child-outputs (chc)
  (let ((sorted-symbols
          (loop for child across (chc:child-symbols chc)
                for child-rel = (find (chc:symbol-name child) (chc:body chc)
                                      :key #'chc:term-actual)
                collect (cons child (elt (chc:output-actuals child-rel) 0))
                  into child-outputs
                finally (return (cl:sort child-outputs #'<
                                         :key (a:compose #'chc:symbol-index #'car))))))
    (loop for (child . output) in sorted-symbols
          collecting (chc:symbol-sort child) into term-types
          collecting output into outputs
          finally (return (values outputs term-types)))))

(defun convert-chc-constraint (cbody chc context)
  "Converts a CHC constraint to a SyGuS production"
  (declare (ignore context))
  (?:match cbody
    ((?:guard (smt:var vname :sort sort) (input-var? vname chc))
     (make-input-production vname sort))
    ((?:access #'smt::find-constants (?:<> (= 0) (length x) x))
     (make-constant-production cbody (smt:sort cbody)))
    (_
     (let ((name (str:concat
                  (smt:identifier-string (chc:name (chc:head chc)))
                  "."
                  (smt:identifier-string (chc:name (chc:constructor chc)))))
           (inputs (delete-if-not (a:rcurry #'input-var? chc)
                                  (smt::find-constants cbody)
                                  :key #'smt:name)))
       (multiple-value-bind (child-outputs term-types)
           (%sort-child-outputs chc)
         (make-function-production name cbody child-outputs term-types inputs))))))
