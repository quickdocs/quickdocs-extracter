(in-package :cl-user)
(defpackage quickdocs-extracter
  (:use :cl
        :quickdocs-parser
        :quickdocs-extracter.parser
        :quickdocs-extracter.data)
  (:shadowing-import-from :quickdocs-extracter.parser
                          :variable-node
                          :method-node)
  (:export :serialize-release
           :serialize-system))
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
                   :readme ,(uiop:read-file-string readme-file))
                 '())))))

(defun serialize-system (system-designator &optional (dist (ql-dist:dist "quicklisp")))
  (let ((system (if (typep system-designator 'ql-dist:system)
                    system-designator
                    (ql-dist:find-system-in-dist system-designator dist)))
        asdf-system)
    (unless system
      (error "System ~S is not found in ~S" system-designator dist))
    (let ((index (parse (ql-dist:name system)))
          packages)
      (do-packages (package index)
        (push (serialize-package package) packages))
      (setf packages (nreverse packages))
      (flet ((ignorable-dependency-p (dep)
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
        (setf asdf-system (asdf:find-system (ql-dist:name system)))
        (list :type :system
              :name (ql-dist:name system)
              :long-name (asdf:system-long-name asdf-system)
              :author (asdf:system-author asdf-system)
              :maintainer (asdf:system-maintainer asdf-system)
              :version (asdf:component-version asdf-system)
              :license (asdf:system-license asdf-system)
              :homepage (asdf:system-homepage asdf-system)
              :bug-tracker (asdf:system-bug-tracker asdf-system)
              :mailto (asdf:system-mailto asdf-system)
              :description (asdf:system-description asdf-system)
              :long-description (asdf:system-long-description asdf-system)
              ;; NOTE: Not using ql::required-systems because it's in random order.
              :depends-on (mapcar #'string-downcase
                                  (remove-if #'ignorable-dependency-p
                                             (mapcar #'dependency-name
                                                     (asdf:component-sideway-dependencies asdf-system))))
              :defsystem-depends-on (mapcar #'string-downcase
                                            (remove-if #'ignorable-dependency-p
                                                       (mapcar #'dependency-name
                                                               (asdf:system-defsystem-depends-on asdf-system))))
              :packages packages)))))

(defun serialize-package (package)
  (check-type package package-index)
  (list :type :package
        :name (package-index-name package)
        :docstring (package-index-docstring package)
        :symbols
        (let (symbols)
          (do-nodes (node package)
            (push (serialize-node node) symbols))
          (nreverse symbols))))

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
    :setfp ,(operator-setf-p node)))

(defmethod serialize-node ((node method-node))
  `(:type :method
    ,@(call-next-method)
    :setfp ,(operator-setf-p node)
    :qualifiers ,(mapcar (lambda (qualifier)
                           (if (keywordp qualifier)
                               qualifier
                               (serialize-symbol qualifier)))
                         (method-node-qualifiers node))))

(defmethod serialize-node ((node generic-function-node))
  `(:type :generic-function
    ,@(call-next-method)
    :setfp ,(operator-setf-p node)))

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
