(defpackage cl-forth
  (:use :cl :cl-utilities))

(in-package :cl-forth)

(defparameter *dictionary* (make-hash-table))
(defvar *state*)