(in-package :cl-user)
(defpackage quickdocs-extracter.data
  (:use :cl)
  (:import-from :quickdocs-parser
                :symbol-external-p)
  (:export :symb
           :symb-name
           :symb-package
           :symb-externalp
           :serialize-symbol
           :serialize-lambda-list
           :serialize-extended-lambda-list
           :serialize-type-specifier
           :serialize-cffi-base-type))
(in-package :quickdocs-extracter.data)

(defstruct symb
  name
  package
  externalp)

(defun serialize-symbol (symbol)
  (check-type symbol symbol)
  (let ((package (symbol-package symbol)))
    (make-symb
     :name (make-symbol (symbol-name symbol))
     :package (and package
                   (make-symbol (package-name package)))
     :externalp (symbol-external-p symbol))))

(defun &-symbol-p (symbol)
  (and (symbolp symbol)
       (eq (symbol-package symbol)
           (find-package :cl))
       (char= #\& (aref (symbol-name symbol) 0))))

(defun take-until (fn list)
  (let ((pos (position-if fn list)))
    (values (subseq list 0 pos)
            (and pos
                 (subseq list pos)))))

(defun serialize-init-form (init-form)
  (prin1-to-string init-form))

(defun serialize-specializer-name (type)
  (etypecase type
    (symbol (serialize-symbol type))
    (list
     (assert (eq (first type) 'eql))
     (list 'eql (serialize-init-form (second type))))))

(defun serialize-extended-lambda-list (lambda-list)
  (check-type lambda-list list)
  (unless lambda-list
    (return-from serialize-extended-lambda-list nil))
  (loop for elem = (pop lambda-list)
        append
        (case elem
          (&allow-other-keys (list '&allow-other-keys))
          ((&optional &key &aux)
           (cons elem
                 (multiple-value-bind (optionals rest)
                     (take-until #'&-symbol-p lambda-list)
                   (setf lambda-list rest)
                   (loop for optional in optionals
                         if (consp optional)
                           collect
                           (let ((var (first optional)))
                             (append
                              (list (etypecase var
                                      (symbol (serialize-symbol var))
                                      (list (mapcar #'serialize-symbol var))))
                              (ecase (length optional)
                                (1 nil)
                                (2 (list (serialize-init-form (second optional))))
                                (3 (list (serialize-init-form (second optional))
                                         (serialize-symbol (third optional)))))))
                         else
                           collect (serialize-symbol optional)))))
          ((&rest &body &whole &environment)
           (let ((var (pop lambda-list)))
             (etypecase var
               (symbol (list elem (serialize-symbol var)))
               (list (list elem (serialize-lambda-list var))))))
          (otherwise
           (etypecase elem
             (symbol (list (serialize-symbol elem)))
             (list (list (serialize-extended-lambda-list elem))))))
        while lambda-list))

(defun serialize-lambda-list (lambda-list)
  (check-type lambda-list list)
  (multiple-value-bind (args rest) (take-until #'&-symbol-p lambda-list)
    (append (loop for arg in args
                  collect
                  (etypecase arg
                    (symbol (serialize-symbol arg))
                    (list (destructuring-bind (var type) arg
                            (list (serialize-symbol var)
                                  (serialize-specializer-name type))))))
            (serialize-extended-lambda-list rest))))

(defun serialize-type-specifier (type-specifier)
  (if (listp type-specifier)
      (destructuring-bind (type &rest args) type-specifier
        (case type
          ((and or)
           (list* type
                  (mapcar #'serialize-type-specifier args)))
          (not
           (assert (null (cdr args)))
           (list 'not (serialize-type-specifier (car args))))
          (otherwise (list* (serialize-symbol type)
                            (mapcar #'serialize-init-form args)))))
      (serialize-symbol type-specifier)))

(defun serialize-cffi-base-type (cffi-base-type)
  (flet ((convert (type)
           (typecase type
             (keyword type)
             (symbol (serialize-symbol type))
             (otherwise (serialize-init-form type)))))
    (if (listp cffi-base-type)
        (mapcar #'convert cffi-base-type)
        (convert cffi-base-type))))
