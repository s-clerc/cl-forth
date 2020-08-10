(in-package :cl-forth)

(define-list-structure (definition (:conc-name def-))
  (name nil :type symbol)
  (sentence nil :type list))

(define-list-structure jump
  (tag nil :type symbol))

(defun resolved-jump-p (?jump)
  (and (jumpp ?jump)
       ;; Make sure that the jump is resolved
       (not (null (jump-tag ?jump)))))

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

(defun run-code (sentence state)
  (when (listp sentence)
    (setf sentence (coerce sentence 'vector)))
  (do* ((i 0 (incf i))
        (element (elt sentence i)
               (when (> (length sentence) i) 
                   (elt sentence i)))
        (controlee (car (state-control state))
                   (car (state-control state))))
       ((or (> i 10) (not element)))
     (setf state
           (with-state state
             (cond 
               ;; Enabling future effects
               ((functionp controlee) 
                (funcall (pop control) state element))
               ;; Used for control flow:
               ((resolved-jump-p controlee)
                (print (car control))
                (print sentence)
                (setf i (position (pop control) sentence))
                (return-state))
               ((jumpp element) 
                (return-state))
               ((functionp element) 
                (funcall element state))
               ;; Call another word
               (t (funcall (ccase (car semantic-mode)
                             (:interpret #'interpret-1)
                             (:compile #'compile-1)
                             (:execute #'execute-1))
                           element state)))))
     (format t "~&~a: ~s ~s" i element state))
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
    (cond ((word token) 
           (handler-case (run-word-function state (word token) #'.compile)
             ;; So we embed it since it isn't immediate
             (word-semantics-null-error (_)
               (push token (third (latest-definition control)))
               (return-state))))
          (t (push token (third (latest-definition control)))
             (return-state)))))

(defun repl (&optional reset)
  (when reset
    (setf *state* (make-state)))
  (loop (fresh-line)
        (princ "Forth SREPL⟩ ")
        (let ((query (read-from-string (read-line))))
          (when (eq query :quit)
            (return))
          (setf *state*
                (run-code (coerce query 'vector) *state*)))))

