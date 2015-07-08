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
           :serialize-lambda-list))
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
            (subseq list pos))))

(defun serialize-init-form (init-form)
  (prin1-to-string init-form))

(defun serialize-lambda-list (lambda-list)
  (check-type lambda-list list)
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
                           collect (ecase (length optional)
                                     (1 (list (serialize-symbol (first optional))))
                                     (2 (list (serialize-symbol (first optional))
                                              (serialize-init-form (second optional))))
                                     (3 (list (serialize-symbol (first optional))
                                              (serialize-init-form (second optional))
                                              (serialize-symbol (third optional)))))
                         else
                           collect (serialize-symbol optional)))))
          ((&rest &body &whole &environment)
           (let ((var (pop lambda-list)))
             (list elem (serialize-symbol var))))
          (otherwise
           (list (serialize-symbol elem))))
        while lambda-list))
