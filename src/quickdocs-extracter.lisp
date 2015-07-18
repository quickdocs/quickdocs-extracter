(in-package :cl-user)
(defpackage quickdocs-extracter
  (:use :cl
        :quickdocs-parser
        :quickdocs-extracter.parser
        :quickdocs-serializer)
  (:shadowing-import-from :quickdocs-extracter.parser
                          :variable-node
                          :method-node)
  (:export :serialize-release
           :serialize-system
           :get-system-basic-info))
(in-package :quickdocs-extracter)

(defun ql-release-version (release)
  "Return Quicklisp dist version the `release' last updated."
  (let ((base-url "http://beta.quicklisp.org/archive/")
        (archive-url (ql-dist:archive-url release)))
    (unless (and (< (length base-url) (length archive-url))
                 (string= base-url  archive-url :end2 (length base-url)))
      (error "Invalid archive-url: ~S" archive-url))
    (subseq archive-url
            (+ (length base-url) 1
               (length (ql-dist:name release)))
            (+ (length base-url) 11
               (length (ql-dist:name release))))))

(defun readme-file (release)
  (check-type release ql-dist:release)
  (assert (ql-dist:installedp release))
  (find "README"
        (uiop:directory-files (ql-dist:base-directory release))
        :key #'pathname-name
        :test #'string=))

(defun serialize-release (release-name &optional (dist (ql-dist:dist "quicklisp")))
  (check-type release-name string)
  (let ((release (ql-dist:find-release-in-dist release-name dist)))
    (unless release
      (error "Release ~S is not found in ~S" release-name dist))
    (ql-dist:ensure-installed release)
    (let ((readme-file (readme-file release)))
      (list* :type :release
             :name release-name
             :release-version (ql-release-version release)
             (if readme-file
                 `(:readme-file ,(subseq (namestring readme-file)
                                         (length (namestring (ql-dist:base-directory release))))
                   :readme
                   ,(handler-bind
                        ((error (lambda (e)
                                  (declare (ignorable e))
                                  #+sbcl
                                  (let ((restart (find-restart 'sb-impl::input-replacement)))
                                    (when restart
                                      (invoke-restart restart
                                                      (or (ignore-errors
                                                           (code-char
                                                            (aref (sb-int:character-decoding-error-octets e) 0)))
                                                          #\?)))))))
                      (uiop:read-file-string readme-file)))
                 '())))))

(defun normalize-author-and-maintainer (author)
  (typecase author
    (string author)
    (null author)
    (cons
     (mapcar (lambda (a)
               (if (stringp a)
                   a
                   (progn
                     (warn "~S in ~S is unexpected type for :author (or :maintainer). Just converting into a string."
                           a
                           author)
                     (princ-to-string a))))
             ;; Some library specifies a quoted list. (ex. '("Scott McKay") in cl-protobufs)
             (if (eq (car author) 'quote)
                 (second author)
                 author)))
    (otherwise
     (warn "~S is unexpected type for :author (or :maintainer). Just converting into a string."
           author)
     (princ-to-string author))))

(defun canonicalize-multi-strings (object)
  "Returns a string or a list of strings"
  (typecase object
    (string object)
    (null '())
    (cons
     (mapcar (lambda (obj)
               (if (stringp obj)
                   obj
                   (progn
                     (warn "~S in ~S cannot be canonicalized as multi-strings. Just converting into a string."
                           obj
                           object)
                     (princ-to-string obj))))
             ;; Some library specifies a quoted list. (ex. '("Scott McKay") in cl-protobufs)
             (if (eq (car object) 'quote)
                 (second object)
                 object)))
    (otherwise
     (warn "~S cannot be canonicalized as multi-strings. Just converting into a string."
           object)
     (princ-to-string object))))

(defun canonicalize-string (object)
  "Returns a string or NIL"
  (typecase object
    (string object)
    (null nil)
    (cons (format nil "~{~A~^~%~}" object))
    (otherwise
     (warn "~S cannot be canonicalized as a string. Just converting into a string."
           object)
     (princ-to-string object))))

(defun find-system-in-dist (system-designator dist)
  (let ((system (if (typep system-designator 'ql-dist:system)
                    system-designator
                    (ql-dist:find-system-in-dist system-designator dist))))
    (unless system
      (error "System ~S is not found in ~S" system-designator dist))
    system))

(defun get-system-basic-info (system-name &optional (dist (ql-dist:dist "quicklisp")))
  (check-type system-name string)
  (labels ((tree-ensure-installed (tree)
             (if (consp tree)
                 (mapc #'tree-ensure-installed tree)
                 (ql-dist:ensure-installed tree)))
           (ignorable-dependency-p (dep)
             (or
              #+sbcl (and (<= 3 (length (string dep)))
                          (string-equal dep "sb-" :end1 3))
              (string-equal dep :asdf)))
           (dependency-name (dep)
             (if (listp dep)
                 (progn
                   (unless (eq (first dep) :version)
                     (error "Unexpected :depends-on: ~S" dep))
                   (second dep))
                 dep)))
    (let ((system (find-system-in-dist system-name dist)))
      (tree-ensure-installed (ql-dist:dependency-tree system))
      (let ((asdf-system
              (handler-case (asdf:find-system (ql-dist:name system))
                (error (e)
                  (return-from get-system-basic-info
                    (list :type :system
                          :name (ql-dist:name system)
                          :failed t
                          :error-log (princ-to-string e)))))))
        (list :type :system
              :name             (canonicalize-string (ql-dist:name system))
              :long-name        (canonicalize-string (asdf:system-long-name asdf-system))
              :author           (canonicalize-multi-strings (asdf:system-author asdf-system))
              :maintainer       (canonicalize-multi-strings (asdf:system-maintainer asdf-system))
              :version          (canonicalize-string (asdf:component-version asdf-system))
              :license          (canonicalize-string (asdf:system-license asdf-system))
              :homepage         (canonicalize-string (asdf:system-homepage asdf-system))
              :bug-tracker      (canonicalize-string (asdf:system-bug-tracker asdf-system))
              :mailto           (canonicalize-string (asdf:system-mailto asdf-system))
              :description      (canonicalize-string (asdf:system-description asdf-system))
              :long-description (canonicalize-string (asdf:system-long-description asdf-system))
              ;; NOTE: Not using ql::required-systems because it's in random order.
              :depends-on
              (delete-duplicates
               (mapcar #'string-downcase
                       (remove-if #'ignorable-dependency-p
                                  (mapcar #'dependency-name
                                          (asdf:component-sideway-dependencies asdf-system))))
               :test #'string=
               :from-end t)
              :defsystem-depends-on
              (delete-duplicates
               (mapcar #'string-downcase
                       (remove-if #'ignorable-dependency-p
                                  (mapcar #'dependency-name
                                          (asdf:system-defsystem-depends-on asdf-system))))
               :test #'string=
               :from-end t))))))

(defun serialize-system (system-designator &optional (dist (ql-dist:dist "quicklisp")))
  (let ((system (find-system-in-dist system-designator dist)))
    (append (get-system-basic-info (ql-dist:name system) dist)
            (let ((index (parse (ql-dist:name system)))
                  packages)
              (do-packages (package index)
                (push (serialize-package package) packages))
              (list :packages (nreverse packages))))))

(defun serialize-package (package)
  (check-type package package-index)
  (list :type :package
        :name (package-index-name package)
        :docstring (package-index-docstring package)
        :symbols
        (let (symbols)
          (do-nodes (node package)
            (push (serialize-node node) symbols))
          (nreverse symbols))
        :reexport-symbols
        (let ((pkg (find-package (package-index-name package)))
              symbols)
          (do-external-symbols (symb pkg (nreverse symbols))
            (unless (eq (symbol-package symb) pkg)
              (push (serialize-symbol symb) symbols))))))

(defgeneric serialize-node (node)
  (:method ((node name-node))
    (list :name (serialize-symbol (node-name node))
          :externalp (and (symbol-package (node-name node))
                          (symbol-external-p (node-name node)))))
  (:method ((node documentation-node))
    (append (call-next-method)
            (list :docstring (and (slot-boundp node 'node-docstring)
                                  (node-docstring node)))))
  (:method ((node operator-node))
    (append (call-next-method)
            (list :lambda-list
                  (let ((*package* (or (symbol-package (node-name node))
                                       *package*)))
                    (funcall
                     (if (typep node 'macro-node)
                         #'serialize-extended-lambda-list
                         #'serialize-lambda-list)
                     (operator-lambda-list node))))))
  (:method ((node record-node))
    (append (call-next-method)
            (list :slots (mapcar #'serialize-node (record-slots node))))))

(defmethod serialize-node ((node function-node))
  `(:type :function
    ,@(call-next-method)
    ,@(if (operator-setf-p node)
          '(:setfp t)
          '())))

(defmethod serialize-node ((node method-node))
  `(:type :method
    ,@(call-next-method)
    ,@(if (operator-setf-p node)
          '(:setfp t)
          '())
    :qualifiers ,(mapcar (lambda (qualifier)
                           (if (keywordp qualifier)
                               qualifier
                               (serialize-symbol qualifier)))
                         (method-node-qualifiers node))))

(defmethod serialize-node ((node generic-function-node))
  `(:type :generic-function
    ,@(call-next-method)
    ,@(if (operator-setf-p node)
          '(:setfp t)
          '())))

(defmethod serialize-node ((node macro-node))
  `(:type :macro ,@(call-next-method)))

(defmethod serialize-node ((node variable-node))
  `(:type :variable
    ,@(call-next-method)
    ,@(if (slot-boundp node 'initial-value)
          (list :initial-value (prin1-to-string (variable-node-initial-value node)))
          '())))

(defmethod serialize-node ((node struct-slot-node))
  (append (list :type :struct-slot)
          (call-next-method)))

(defmethod serialize-node ((node class-slot-node))
  `(:type :class-slot
    ,@(call-next-method)
    :accessors ,(mapcar #'serialize-symbol (slot-accessors node))
    :readers   ,(mapcar #'serialize-symbol (slot-readers node))
    :writers   ,(mapcar #'serialize-symbol (slot-writers node))
    :slot-type ,(and (slot-boundp node 'type)
                     (serialize-type-specifier (slot-type node)))
    :allocation ,(slot-allocation node)))

(defmethod serialize-node ((node struct-node))
  `(:type :struct
    ,@(call-next-method)))

(defmethod serialize-node ((node class-node))
  `(:type :class
    ,@(call-next-method)
    :superclasses ,(mapcar #'serialize-symbol (class-node-superclasses node))))

(defmethod serialize-node ((node condition-node))
  (let ((result (call-next-method)))
    (setf (getf result :type) :condition)
    result))

(defmethod serialize-node ((node type-node))
  (let ((result (call-next-method)))
    (setf (getf result :type) :type)
    result))

(defmethod serialize-node ((node cffi-function))
  (let ((result (call-next-method)))
    (setf (getf result :type) :cffi-function)
    (append result
            (list :return-type (serialize-anything (cffi-function-return-type node))))))

(defmethod serialize-node ((node cffi-type))
  `(:type :cffi-type
    ,@(call-next-method)
    :base-type ,(serialize-anything (cffi-type-base-type node))))

(defmethod serialize-node ((node cffi-slot))
  `(:type :cffi-slot
    ,@(call-next-method)
    :slot-type ,(serialize-type-specifier (cffi-slot-type node))))

(defmethod serialize-node ((node cffi-struct))
  `(:type :cffi-struct
    ,@(call-next-method)
    :slots ,(mapcar #'serialize-node (cffi-struct-slots node))))

(defmethod serialize-node ((node cffi-union))
  `(:type :cffi-union
    ,@(call-next-method)
    :variants ,(mapcar #'serialize-node (cffi-union-variants node))))

(defmethod serialize-node ((node cffi-enum))
  `(:type :cffi-enum
    ,@(call-next-method)
    :variants ,(cffi-enum-variants node)))

(defmethod serialize-node ((node cffi-bitfield))
  `(:type :cffi-bitfield
    ,@(call-next-method)
    :masks ,(cffi-bitfield-masks node)))
