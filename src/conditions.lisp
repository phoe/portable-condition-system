;;;; src/conditions.lisp

(in-package #:portable-condition-system)

;;; Class CONDITION

(defclass condition () ()
  (:documentation "The base condition type that is the supertype of all
condition objects."))

(defmethod print-object ((condition condition) stream)
  "Default condition reporting method which prints the condition type."
  (format stream "Condition ~S was signaled." (type-of condition)))

(defmethod print-object :around ((condition condition) stream)
  "Prints of reports a condition to the provided stream. If *PRINT-ESCAPE* is
bound, the condition is print unreadably; otherwise, it is reported by means of
calling the next printing method."
  (if *print-escape*
      (print-unreadable-object (condition stream :type t :identity t))
      (call-next-method)))

;;; DEFINE-CONDITION

(defun make-condition (type &rest args)
  "Instantiates a new condition object of the provided type with the provided
arguments."
  (apply #'make-instance type args))

(defun expand-define-condition-report-method (name report-option)
  "Accepts the name of the condition being defined and the report option
provided to DEFINE-CONDITION, and returns a DEFMETHOD PRINT-OBJECT form meant to
be spliced into the generated DEFINE-CONDITION expansion."
  (let* ((condition (gensym "CONDITION"))
         (stream (gensym "STREAM"))
         (report (second report-option))
         (report-form (if (stringp report)
                          `(write-string ,report ,stream)
                          `(funcall #',report ,condition ,stream))))
    `(defmethod print-object ((,condition ,name) ,stream)
       ,report-form)))

(defun expand-define-condition-remove-report-method (name)
  "Accepts the method name and expands into a form which removes any
PRINT-OBJECT method defined on the class named by NAME."
  (let ((method (gensym "CONDITION")))
    `(let ((,method (find-method #'print-object '() '(,name t) nil)))
       (when ,method (remove-method #'print-object ,method)))))

(defvar *in-define-condition-p* nil
  "This will be dynamically binded to indicate an entry into DEFINE-CONDITION,
to distinguish its DEFCLASS from an external DEFCLASS.")

(defmethod ensure-class-using-class :before (class name &rest args &key direct-superclasses &allow-other-keys)
  "In DEFINE-CONDITION: Signals a TYPE-ERROR at the first direct superclass unconforming
with DEFINE-CONDITION, that is, the first encountered superclass in DIRECT-SUPERCLASSES
that is not a subtype of CONDITION.
In DEFCLASSes external to DEFINE-CONDITION: Signals an INVALID-SUPERCLASS at the first
encountered superclass that is subtype of CONDITION."
  (declare (ignore args))
  (if *in-define-condition-p*
      (loop :for superclass :in direct-superclasses
            :do (when (not (subtypep superclass 'condition))
                  (error 'type-error
                         :datum superclass
                         :expected-type 'condition)))
      (loop :for superclass :in direct-superclasses
            :do (when (subtypep superclass 'condition)
                  (error 'invalid-superclass
                         :class name
                         :superclass superclass
                         :reason (format nil "Only ~S should~% define a subtype of ~S"
                                         'define-condition 'condition))))))

(defun expand-define-condition (name supertypes direct-slots options)
  "Defines a new condition type via DEFCLASS, handling the :REPORT options via
defining a PRINT-object method on the newly created class."
  (let* ((report-option (find :report options :key #'car))
         (other-options (remove report-option options))
         (supertypes (or supertypes '(condition))))
    `(progn
       (let ((*in-define-condition-p* t))
         (defclass ,name ,supertypes ,direct-slots ,@other-options))
       ,@(if report-option
             `(,(expand-define-condition-report-method name report-option))
             `(,(expand-define-condition-remove-report-method name)))
       ',name)))

(defmacro define-condition (name (&rest supertypes) direct-slots &rest options)
  "Defines or redefines a condition type."
  (expand-define-condition name supertypes direct-slots options))
