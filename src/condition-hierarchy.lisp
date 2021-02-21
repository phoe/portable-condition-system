;;;; src/conditions.lisp

(in-package #:portable-condition-system)

;;; Standard conditions
;;;
;;; The standard condition types defined within this file are not documented via
;;; documentation strings due to the repetitiveness of the contents of this
;;; file.

(define-condition-internal warning (condition) ())

(define-condition-internal serious-condition (condition) ())

(define-condition-internal error (serious-condition) ())

(define-condition-internal style-warning (warning) ())

(defun report-simple-condition (condition stream)
  (let ((format-control (simple-condition-format-control condition))
        (format-args (simple-condition-format-arguments condition)))
    (if format-control
        (apply #'format stream format-control format-args)
        (format stream "Condition ~S was signaled with format arguments ~S."
                (type-of condition) format-args))))

(define-condition-internal simple-condition ()
  ((format-control :reader simple-condition-format-control
                   :initarg :format-control)
   (format-arguments :reader simple-condition-format-arguments
                     :initarg :format-arguments))
  (:default-initargs :format-control nil :format-arguments '())
  (:report report-simple-condition))

(define-condition-internal simple-warning (simple-condition warning) ())

(define-condition-internal simple-error (simple-condition error) ())

(define-condition-internal storage-condition (serious-condition) ())

(defun report-type-error (condition stream)
  (format stream "~@<The value ~@:_~2@T~S ~@:_is not of type ~@:_~2@T~S.~:@>"
          (type-error-datum condition)
          (type-error-expected-type condition)))

(define-condition-internal type-error (error)
  ((datum :reader type-error-datum :initarg :datum)
   (expected-type :reader type-error-expected-type :initarg :expected-type))
  (:report report-type-error))

(define-condition-internal simple-type-error (simple-condition type-error) ())

(define-condition-internal control-error (error) ())

(define-condition-internal program-error (error) ())

(define-condition-internal cell-error (error)
  ((name :reader cell-error-name :initarg :name)))

(defun report-unbound-variable (condition stream)
  (format stream "The variable ~S is unbound."
          (cell-error-name condition)))

(define-condition-internal unbound-variable (cell-error) ()
  (:report report-unbound-variable))

(defun report-undefined-function (condition stream)
  (format stream "The function ~S is undefined."
          (cell-error-name condition)))

(define-condition-internal undefined-function (cell-error) ()
  (:report report-undefined-function))

(defun report-unbound-slot (condition stream)
  (format stream "The slot ~S is unbound in ~S."
          (cell-error-name condition)
          (unbound-slot-instance condition)))

(define-condition-internal unbound-slot (cell-error)
  ((instance :reader unbound-slot-instance :initarg :instance))
  (:report report-unbound-slot))

(define-condition-internal stream-error (error)
  ((stream :reader stream-error-stream :initarg :stream)))

(define-condition-internal end-of-file (stream-error) ())

(define-condition-internal parse-error (error) (stream))

(define-condition-internal reader-error (parse-error stream-error) ())

(define-condition-internal package-error (error)
  ((package :reader package-error-package :initarg :package)))

(define-condition-internal arithmetic-error (error)
  ((operation :reader operation-error-operation :initarg :operation)
   (operands :reader operands-error-operands :initarg :operands)))

(define-condition-internal division-by-zero (arithmetic-error) ())

(define-condition-internal floating-point-invalid-operation (arithmetic-error) ())

(define-condition-internal floating-point-inexact (arithmetic-error) ())

(define-condition-internal floating-point-overflow (arithmetic-error) ())

(define-condition-internal floating-point-underflow (arithmetic-error) ())

(define-condition-internal file-error (error)
  ((pathname :reader pathname-error-pathname :initarg :pathname)))

(define-condition-internal print-not-readable (error)
  ((object :reader print-not-readable-object :initarg :object)))

;;; Non-standard condition types

(define-condition-internal restart-not-found (control-error)
  ((restart-name :reader restart-not-found-restart-name :initarg :restart-name))
  (:documentation "A condition type signaled when a restart with a given name
was not found, even thought it was expected.")
  (:report (lambda (condition stream)
             (format stream "Restart ~S is not active."
                     (restart-not-found-restart-name condition)))))

(define-condition-internal abort-failure (control-error) ()
  (:documentation "A condition type signaled when the ABORT restart invoked by
function ABORT failed to transfer control outside of the function.")
  (:report "An ABORT restart failed to transfer control."))

(defun report-case-failure (condition stream)
  (format stream "~S fell through ~S expression.~%Wanted one of ~:S."
          (type-error-datum condition)
          (case-failure-name condition)
          (case-failure-possibilities condition)))

(define-condition-internal case-failure (type-error)
  ((name :reader case-failure-name :initarg :name)
   (possibilities :reader case-failure-possibilities :initarg :possibilities))
  (:documentation "A condition type signaled when a case assertion
(such as ECASE, ETYPECASE, CCASE, or CTYPECASE) fails to match its keyform.")
  (:report report-case-failure))
