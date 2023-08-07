;;;;
;;;; SyGuS problem information
;;;;
(in-package #:systems.duck.ks2.sygus)

(defclass production-info ()
  ((sort :reader sort
         :initarg :sort
         :type smt:sort
         :documentation "The function output sort")))

(defgeneric render-info (info occurrences)
  (:documentation "Renders a PRODUCTION-INFO object with the given occurrences"))

(defgeneric smt-declaration (info context)
  (:documentation "Renders a declaration string for INFO. Possibly NIL.")
  (:method (info context) nil))

;;;
;;; Function productions - a function call in the SyGuS grammar
;;;
(defclass function-production (production-info)
  ((name :reader name
         :initarg :name
         :type symbol
         :documentation "The function name")
   (body :reader body
         :initarg :body
         :type smt::expression
         :documentation "The function body")
   (child-outputs :reader child-outputs
                  :initarg :child-outputs
                  :type list
                  :documentation "The children output names involved")
   (term-types :reader term-types
               :initarg :term-types
               :type list
               :documentation "The term types matching with the child outputs")
   (inputs :reader inputs
           :initarg :inputs
           :type list
           :documentation "Input variables used in this production")))

(defun make-function-production (name body child-outputs term-types inputs)
  (make-instance 'function-production
                 :name name
                 :sort (smt:sort body)
                 :body body
                 :child-outputs child-outputs
                 :term-types term-types
                 :inputs inputs))

(defmethod print-object ((object function-production) stream)
  (print-unreadable-object (object stream :type t)
    (format stream "~a: ~a(~a)[~a] -> ~a"
            (smt:name (sort object))
            (name object)
            (str:join "," (map 'list (a:compose #'first #'smt:name)
                               (term-types object)))
            (str:join "," (map 'list #'smt:identifier-string (inputs object)))
            (smt:to-smt (body object) :pprint t))))

(defmethod render-info ((fp function-production) occurrences)
  (let ((args (append (map 'list #'g:name occurrences)
                      (inputs fp))))
    (if (null args)
        (format nil "~a" (smt:identifier-string (name fp)))
        (format nil "(~a~{ ~a~^~})"
                (smt:identifier-string (name fp))
                (map 'list #'smt:identifier-string args)))))

(defmethod smt-declaration ((fp function-production) context)
  (smt::function-declaration
   (name fp)
   (append (map 'list (a:rcurry #'tt-sort context) (term-types fp))
           (map 'list #'smt:sort (inputs fp)))
   (sort fp)
   (append (child-outputs fp)
           (map 'list #'smt:name (inputs fp)))
   (body fp)))

;;;
;;; Input productions - references to an input variable
;;;
(defclass input-production (production-info)
  ((variable :reader variable
             :initarg :variable
             :type symbol
             :documentation "The variable name")))

(defun make-input-production (variable sort)
  "Makes an INPUT-PRODUCTION production information"
  (make-instance 'input-production :variable variable :sort sort))

(defmethod print-object ((object input-production) stream)
  (print-unreadable-object (object stream :type t)
    (format stream "~a: ~a"
            (smt:name (sort object))
            (smt:identifier-string (variable object)))))

(defmethod render-info ((ip input-production) occurrences)
  (assert (null occurrences))
  (format nil "~a" (smt:identifier-string (variable ip))))

;;;
;;; Constant productions - an expression not containing inputs or child outputs
;;;
(defclass constant-production (production-info)
  ((value :reader value
          :initarg :value
          :type smt::expression
          :documentation "The production expression")))

(defun make-constant-production (value sort)
  "Makes a CONSTANT-PRODUCTION production information"
  (make-instance 'constant-production :value value :sort sort))

(defmethod print-object ((object constant-production) stream)
  (print-unreadable-object (object stream :type t)
    (format stream "~a: ~a"
            (smt:name (sort object))
            (smt:to-smt (value object) :pprint t))))

(defmethod render-info ((cp constant-production) occurrences)
  (assert (null occurrences))
  (smt:to-smt (value cp) :pprint t))
