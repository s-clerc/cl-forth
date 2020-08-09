(in-package :cl-forth)

(define-list-structure (definition (:conc-name def-))
  (name nil :type symbol)
  (sentence nil :type list))

(defun latest (stack identifier)
  (unless (functionp identifier)
    (let ((keyword identifier))
      (setf identifier #'(lambda (sample)
                             (if (listp sample)
                               (eq (car sample) keyword)
                               (eq sample keyword))))))
  (loop for sample in stack 
        when (funcall identifier sample)
          return sample))

(defun latest-definition (control)
  (latest control #'definitionp))

(defun run-code (tokens state)
  (dolist (?token tokens)
     (setf state
           (with-state state
             (if (functionp ?token) 
                 (funcall ?token state)
                 (ccase semantic-mode
                        (:interpret (interpret-1 ?token state))
                        (:compile (compile-1 ?token state))
                        (:execute (execute-1 ?token state))))))
     (format t "~&~s ~s" ?token state))
  state)

(define-condition word-semantics-null-error (error)
  ((state :initarg :state)
   (given-accessor :initarg :accessor)
   (word :initarg :word)))

(defun run-word-function (state word accessor)
  "(conditionally state)
Basically it runs the correct function based on the word settings"
  (let ((function (funcall accessor word)))
    (case function
      (:execute (funcall (.execute word) state))
      ('nil (error 'word-semantics-null-error
                   :state state
                   :word word
                   :accessor accessor) state)
      (t (funcall function state)))))

(defun interpret-1 (token state)
  "=> state"
  (with-state state
    (cond ((word token)
             (run-word-function state (word token) #'.interpret))
          (t (push token data)
             (return-state)))))

(defun execute-1 (token state)
  "=> state"
  (with-state state
    (cond ((word token)
             (run-word-function state (word token) #'.execute))
          (t (push token data)
             (return-state)))))

(defun compile-1 (token state)
  "Compile one token"
  (with-state state
    ;;; This section will see if anything in the control stack requires
    ;;; immediate action
    (or
      ;;; This section will handle tokens in general
      (cond
        ((functionp (car control)) 
           (funcall (pop control) state token))
        ((word token) 
           (handler-case (run-word-function state (word token) #'.compile)
             ;; So we embed it since it isn't immediate
             (word-semantics-null-error (_)
               (push token (third (latest-definition control)))
               (return-state))))
        (t (push token (third (latest-definition control)))
           (return-state))))))

(defun repl (&optional reset)
  (when reset
    (setf *state* (make-state)))
  (loop (fresh-line)
        (princ "Forth SREPL⟩ ")
        (let ((query (read-from-string (read-line))))
          (when (eq query :quit)
            (return))
          (setf *state*
                (run-code query *state*)))))

