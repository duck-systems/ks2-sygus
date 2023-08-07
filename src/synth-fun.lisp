;;;;
;;;; A SyGuS-style synthfun
;;;;
(in-package #:systems.duck.ks2.sygus)

(defclass synth-fun ()
  ((name :reader name
         :initarg :name
         :type symbol
         :documentation "The name of the function to synthesize")
   (formals :reader formals
            :initarg :formals
            :type list ; of smt:constant
            :documentation "The input formals to the function to synthesize")
   (sort :reader sort
         :initarg :sort
         :type smt:sort
         :documentation "The output sort of the function to synthesize")
   (non-terminals :reader non-terminals
                  :initarg :non-terminals
                  :type list ; of (modified) non-terminals
                  :documentation "Non-terminals in this grammar")
   (productions :reader productions
                :initarg :productions
                :type list ; of production-information objects
                :documentation "Productions in this grammar")
   (smt-fn :reader smt-fn
           :initarg :smt-fn
           :type smt::function
           :documentation "Function declaration"))
  (:documentation "Describes the function to synthesize, SyGuS-style"))

(defclass sygus-non-terminal (g:non-terminal)
  ((original-non-terminal :reader original-non-terminal
                          :initarg :original
                          :type g:non-terminal
                          :documentation "The original non-terminal created from")))

(defclass sygus-production ()
  ((info :reader info
         :initarg :info
         :type production-info
         :documentation "Info object about this production")
   (original-instance :reader original-instance
                      :initarg :original-instance
                      :type g:non-terminal
                      :documentation "The original non-terminal instance")
   (original-occurrences :reader original-occurrences
                         :initarg :original-occurrences
                         :type list ; of g:non-terminal
                         :documentation "The original occurrences")))

(defmethod print-object ((object sygus-production) stream)
  (print-object (info object) stream))

(defun %convert-non-terminal (nt context)
  "Converts a non-terminal NT from SemGuS-style to SyGuS-style"
  ;; It's nice if we can use a friendly name for non-terminals
  ;; But we can only do it when there isn't any ambiguity (can be improved)
  (let ((same-typed-nts (g:non-terminals-for-term-type context (g:term-type nt)))
        (name (g:name nt)))
    (when (= 1 (length same-typed-nts))
      (setf name (smt:ensure-identifier (smt:name (g:term-type nt)))))

    (make-instance 'sygus-non-terminal
                   :original nt
                   :name name
                   :term-type (tt-sort (g:term-type nt) context))))

(defun %convert-production (prod context)
  "Converts a production PROD from SemGuS-style to SyGuS-style"
  (let ((chcs (semgus:lookup-chcs-by-operator (g:operator prod) context)))
    (assert (= 1 (length chcs)))
    (make-instance 'sygus-production
                   :original-instance (g:instance prod)
                   :original-occurrences (g:occurrences prod)
                   :info (chc:get-data :sygus-prod (first chcs)))))

(defun make-synth-fun (context)
  "Makes a SyGuS synth-fun from CONTEXT, a SemGuS context"
  (let ((name (semgus:term-name context))
        (sort (tt-sort (semgus:term-type context) context))
        (head (first (semgus:root-relations context)))
        (nts (map 'list (a:rcurry #'%convert-non-terminal context)
                  (g:non-terminals context)))
        (prods (map 'list (a:rcurry #'%convert-production context)
                    (g:productions context))))
    (make-instance 'synth-fun
                   :name name
                   :sort sort
                   :formals (map 'list #'(lambda (s)
                                           (smt:variable (chc:symbol-name s)
                                                         (chc:symbol-sort s)))
                                 (chc:input-symbols head))
                   :non-terminals nts
                   :productions prods
                   :smt-fn (smt::function-declaration
                            name
                            (map 'list #'chc:symbol-sort (chc:input-symbols head))
                            sort))))

(defmethod smt:to-smt ((sf synth-fun) &key pprint)
  (declare (ignore pprint))
  (format nil "(synth-fun ~A ~A ~A ~%  ~A~%  ~A)"
          (smt:identifier-string (name sf))
          (map 'list #'(lambda (iv)
                         (format nil "(~a ~a)"
                                 (smt:identifier-string (smt:name iv))
                                 (smt:name (smt:sort iv))))
               (formals sf))
          (smt:name (sort sf))
          (map 'list #'(lambda (nt)
                         (format nil "(~a ~a)"
                                 (smt:identifier-string (g:name nt))
                                 (smt:name (g:term-type nt))))
               (non-terminals sf))
          (map 'list #'(lambda (nt)
                         (let ((prods (remove-if-not
                                       (a:curry #'eql (original-non-terminal nt))
                                       (productions sf)
                                       :key #'original-instance)))
                           (format nil "(~a ~a ~a)"
                                   (smt:identifier-string (g:name nt))
                                   (smt:name (g:term-type nt))
                                   (map 'list #'(lambda (p)
                                                  (render-info
                                                   (info p)
                                                   (map 'list
                                                        #'(lambda (x)
                                                            (find
                                                             x
                                                             (non-terminals sf)
                                                             :key
                                                             #'original-non-terminal))
                                                        (original-occurrences p))))
                                        prods))))
               (non-terminals sf))))
