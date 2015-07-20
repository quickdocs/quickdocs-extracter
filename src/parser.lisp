(in-package :cl-user)
(defpackage quickdocs-extracter.parser
  (:use :cl
        :quickdocs-parser)
  (:shadow :variable-node
           :method-node
           :struct-node)
  (:export :variable-node
           :constant-node
           :method-node
           :struct-node
           :initial-value
           :variable-node-initial-value
           :method-node-qualifiers
           :struct-node-include-structs))
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

(defclass struct-node (quickdocs-parser:struct-node)
  ((include-structs :initarg :include-structs
                    :initform nil
                    :reader struct-node-include-structs)))

(define-parser cl:defstruct (name-and-options &rest slots)
  (let* ((name (if (listp name-and-options)
                   (first name-and-options)
                   name-and-options))
         (includes (if (listp name-and-options)
                       (mapcan #'cdr
                               (remove-if-not
                                (lambda (option)
                                  (and (consp option)
                                       (eq (first option) :include)))
                                (rest name-and-options)))
                       nil))
         (docstring (if (stringp (first slots))
                        (first slots)
                        nil))
         (slots (if (stringp (first slots))
                    (rest slots)
                    slots)))
    (make-instance 'struct-node
                   :name name
                   :docstring docstring
                   :slots (loop for slot in slots
                                collecting (quickdocs-parser::parse-struct-slot slot))
                   :include-structs includes)))
