;;;; t/more-tests.lisp

(in-package #:portable-condition-system/test)

;;; Test superclassing a CONDITION in DEFCLASS

(defclass not-a-condition-0 nil nil)

(define-condition-with-tests condition-in-super-1a nil nil)
(define-condition-with-tests condition-in-super-1b nil nil)

(deftest condition-in-defclass-superclass-1.1
    (signals-error (defclass condition-in-defclass-superclass-1a (condition-in-super-1a)
                     ())
                   portable-condition-system::invalid-superclass)
  t)

(deftest condition-in-defclass-superclass-1.2
    (signals-error (defclass condition-in-defclass-superclass-1b (not-a-condition-0
                                                                  condition-in-super-1b
                                                                  condition-in-super-1a)
                     ())
                   portable-condition-system::invalid-superclass)
  t)

;;;
