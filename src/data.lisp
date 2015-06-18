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
           :serialize-list))
(in-package :quickdocs-extracter.data)

(defstruct symb
  name
  package
  externalp)

(defun serialize-symbol (symbol)
  (check-type symbol symbol)
  (let ((package (symbol-package symbol)))
    (apply #'make-symb
           :name (symbol-name symbol)
           :package
           (and package
                (package-name package))
           (if package
               (list :externalp (symbol-external-p symbol))
               '()))))

(defun serialize-list (list)
  (mapcar (lambda (item)
            (typecase item
              (null nil)
              (cons (serialize-list item))
              ((and symbol
                    (not keyword))
               (serialize-symbol item))
              (otherwise item)))
          list))
