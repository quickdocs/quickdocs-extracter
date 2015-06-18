(in-package :cl-user)
(defpackage quickdocs-extracter.parser
  (:use :cl
        :quickdocs-parser)
  (:shadow :variable-node
           :method-node)
  (:export :variable-node
           :constant-node
           :method-node
           :initial-value
           :variable-node-initial-value
           :method-node-qualifiers))
(in-package :quickdocs-extracter.parser)

(defclass variable-node (quickdocs-parser:variable-node)
  ((initial-value :initarg :initial-value
                  :reader variable-node-initial-value)))

(defclass constant-node (variable-node) ())

(define-parser cl:defconstant (name value &optional documentation)
  (make-instance 'constant-node
                 :name name
                 :initial-value value
                 :docstring documentation))

(define-parser cl:defvar (name &optional (value nil value-specified-p) documentation)
  (apply #'make-instance 'variable-node
         :name name
         :docstring documentation
         (if value-specified-p
             (list :initial-value value)
             '())))

(define-parser cl:defparameter (name &optional (value nil value-specified-p) documentation)
  (apply #'make-instance 'variable-node
         :name name
         :docstring documentation
         (if value-specified-p
             (list :initial-value value)
             '())))

(defclass method-node (quickdocs-parser:method-node)
  ((qualifiers :initarg :qualifiers
               :initform nil
               :reader method-node-qualifiers)))

(define-parser cl:defmethod (name &rest args)
  (let* ((lambda-list-pos (position-if #'listp args))
         (qualifiers (subseq args 0 lambda-list-pos))
         (lambda-list (nth lambda-list-pos args))
         (body (subseq args (1+ lambda-list-pos)))
         (docstring
           (loop for exp in body
                 if (and (not (stringp exp))
                         (not (listp exp))
                         (eq (car exp) 'cl:declare))
                   do (return)
                 else if (stringp exp)
                        do (return exp))))
    (make-instance 'method-node
                   :name (if (listp name)
                             (second name)
                             name)
                   :setfp (listp name)
                   :qualifiers qualifiers
                   :lambda-list lambda-list
                   :docstring docstring)))
