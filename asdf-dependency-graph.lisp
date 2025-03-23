(defpackage #:asdf-dependency-graph
  (:use #:common-lisp)
  (:export #:generate
           #:*interesting-systems*))

(in-package #:asdf-dependency-graph)

(defstruct node parent child type (condition ""))

(defun name-from-system-description (system-description)
  (flet ((feature-string (requirement)
           (format nil "(:feature ~A)" (string-downcase (etypecase requirement
                                                          ((eql nil) "")
                                                          (string requirement)
                                                          (list   (write-to-string requirement))
                                                          (symbol (symbol-name requirement)))))))
    (optima:match system-description
      ((list* :feature requirement (list :require name) _)
       (values name :require (feature-string requirement)))
      ((list* :feature requirement name _)
       (values name :asdf (feature-string requirement)))
      ((list* :version name _)
       (values name :asdf nil))
      (_
       (values system-description :asdf nil)))))

(defun node-name-from-name-type (name type primary-only)
  (ecase type
    (:require (format nil "(:require ~A)" (string-downcase (string name))))
    (:asdf (cond (primary-only (asdf:primary-system-name name))
                 ((typep name 'asdf:component) (asdf:component-name name))
                 (t name)))))

(defun same-primary-system-p (parent child)
  (equal (asdf:primary-system-name parent) (asdf:primary-system-name child)))

(defun next-depth (current-depth parent child)
  (when current-depth
    (- current-depth
       ;; Avoid incrementing the current depth for, say, PACKAGE-INFERRED-SYSTEMS from a single
       ;; system definition
       (if (same-primary-system-p parent child)
           0
           1))))

(defun resolve-system (name type)
  (when (eq type :asdf)
    (asdf:find-system name)))

(defun node-system-dependencies (node)
  (case (node-type node)
    (:asdf
     (asdf:system-depends-on (asdf:find-system (node-parent node))))))

(defun dependency-node-list (asdf-system &optional extant-depth primary-only)
  (declare (type asdf:system asdf-system))
  (when (or (null extant-depth) (> extant-depth 0))
    (let ((child-name (asdf:primary-system-name asdf-system))
          (dependencies (asdf:system-depends-on asdf-system)))
      (loop :for system-description :in dependencies
            :nconcing (multiple-value-bind (system-name type requirement)
                          (name-from-system-description system-description)
                        (let* ((system (resolve-system system-name type))
                               (parent-name (node-name-from-name-type
                                             (or system system-name) type primary-only))
                               (tail (ecase type
                                       (:require nil)
                                       (:asdf (dependency-node-list
                                               system
                                               (next-depth extant-depth asdf-system system)
                                               primary-only)))))
                          (if (or (null system) (same-primary-system-p asdf-system system))
                              tail ; avoid nodes pointing to themselves
                              (cons (make-node :parent parent-name
                                               :child child-name
                                               :type type
                                               :condition (or requirement ""))
                                    tail))))))))

(defvar *interesting-systems*)
(setf (documentation '*interesting-systems* 'variable)
      "If bound, the generated graph contains nodes corresponding only to the systems
mentioned in *INTERESTING-SYSTEMS*")
(declaim (type list *interesting-systems*))

(defun may-be-filter-for-interesting-systems (node-list primary-only)
  (if (not (boundp '*interesting-systems*))
      node-list
      (flet ((all-nodes-are-interesting-p (node-list)
               (loop :for node :in node-list
                     :always
                     (and (member (node-parent node)
                                  *interesting-systems* :test #'string=)
                          (member (node-child  node)
                                  *interesting-systems* :test #'string=)))))
        (let ((*interesting-systems*
                (loop :for system :in *interesting-systems*
                      :collect (etypecase system
                                 (asdf:system (asdf:primary-system-name system))
                                 (string system)))))
          (loop :until (all-nodes-are-interesting-p node-list)
                :do (setq node-list
                          ;; - If both parent and child are interesting, keep the node.
                          ;; - If only the child is interesting, ignore this node, but
                          ;;   add the nodes connecting child to the parent-of-parents.
                          ;;   Eventually, over multiple rounds, all the nodes will
                          ;;   become interesting.
                          ;; - If the child is not interesting, then two ways to reach
                          ;;   this state include
                          ;;   (i)  child was a target-system, in which case, we are
                          ;;        not bothered about its ancestors at all.
                          ;;   (ii) child was not a target-system. FIXME: What about this case?
                          (loop :for node :in node-list
                                :nconcing
                                (cond ((and (member (node-parent node)
                                                    *interesting-systems* :test #'string=)
                                            (member (node-child  node)
                                                    *interesting-systems* :test #'string=))
                                       (list node))
                                      ((member (node-child node) *interesting-systems* :test #'string=)
                                       (loop :for system-description
                                               :in (node-system-dependencies node)
                                             :collect
                                             (multiple-value-bind (name type requirement)
                                                 (name-from-system-description system-description)
                                               (make-node :parent (node-name-from-name-type name type primary-only)
                                                          :child (node-child node)
                                                          :type type
                                                          :condition (or requirement "")))))))))
          node-list))))

(defun generate (output-file target-system &key maximum-depth primary-only)
  (let* ((target-system (etypecase target-system
                          ((or string symbol) (asdf:find-system target-system))
                          (asdf:system target-system)))
         (img-format (let ((suffix-start (position #\. output-file :from-end t)))
                       (if suffix-start
                           (subseq output-file (1+ suffix-start))
                           "png")))
         (node-list (may-be-filter-for-interesting-systems
                     (dependency-node-list
                      target-system maximum-depth primary-only)
                     primary-only)))
    (uiop:with-temporary-file (:type "gv" :pathname path)
      (with-open-file (f path :direction :output
                              :if-exists :supersede)
        (write-string "strict digraph {" f)
        (write-string "rankdir=LR;" f)
        (dolist (node node-list)
          (with-slots (parent child condition) node
            (format f "  \"~A\" -> \"~A\" [ label=\"~A\" ];" parent child condition)
            (terpri f)))
        (write-string "}" f))
      (uiop:run-program (format nil "dot -T~A '~A' > '~A'"
                                img-format
                                (namestring path)
                                output-file)
                        :error-output *error-output*
                        :output *standard-output*))))
