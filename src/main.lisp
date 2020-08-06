(defpackage cl-forth
  (:use :cl :cl-utilities))

(in-package :cl-forth)

(defparameter *dictionary* (make-hash-table))

(defmacro word (name)
  `(gethash ,name *dictionary*))

(defmacro wordq (name)
  `(word ',name))

(declaim (ignore _))

(defclass word () 
  ((name        
     :initform nil
     :type symbolp
     :accessor .name)    
   (execute
     :initform #'identity
     :type functionp
     :accessor .execute)    
   (interpret
     :initform #'identity
     :accessor .interpret
     :type (or functionp keywordp))
   (compile
     :initform #'identity
     :accessor .compile
     :type (or functionp keywordp))))

(defun flag (boolean)
  "Lisp Boolean to Forth Flag"
  (if boolean -1 0))

(defun deflag (flag)
  "Forth flag to Lisp Boolean"
  (if (= flag 0) nil t))

(defstruct (state (:type list) :named (:predicate statep))
  (data nil :type list)
  (control nil :type list)
  (return nil :type list)
  (semantic-mode :interpret :type (member :interpret 
                                          :compile 
                                          :execute)))

;; Macro because `and` is not a function :(
(defmacro reflag (operator &rest arguments)
  "Converts arguments to lisp booleans, applies operator to them, and then
converts the result back to a flag"
  `(flag (,operator ,@(mapcar #'(lambda (argument) `(deflag ,argument)) arguments))))

(defconstant +lambda-list+ '(data control return semantic-mode))

(defmacro with-state (state &body body)
  (with-gensyms (ignore)
    `(destructuring-bind (,ignore ,@+lambda-list+) ,state
        (declare (ignore ,ignore))
        (declare (ignorable ,@+lambda-list+))
       ,@body)))

(defmacro return-state ()
  `(list 'state ,@+lambda-list+))

(defmacro make-word (name execute &optional (interpret :execute) (compile nil))
  (with-gensyms (result)  
    `(let ((,result (make-instance 'word)))
       (setf (.name ,result) ,name)
       (setf (.execute ,result) ,execute)
       (setf (.interpret ,result) ,interpret)
       (setf (.compile ,result) ,compile)
       (setf (gethash (.name ,result) *dictionary*) ,result))))

(defmacro defword (name execute &optional (interpret :execute) (compile nil))
  (labels ((parse-specs (specs)
             ;; So if there is only one spec, only (:s ...) and not ((:s ...)) may be written
             (when (not (listp (first specs))) 
               (setf specs (list specs)))
             (let* ((specs (loop for spec in specs 
                                 collect (split-sequence '-- spec)))
                    (variables (remove-duplicates (loop for spec in specs 
                                                        append (rest (first spec))))))
               (with-gensyms (state)
                  `(lambda (,state) 
                     (with-state ,state
                        (let (,@(remove '_ variables))
                          ,@(loop for (stack-and-variables mapping) in specs
                                  for stack = (case (first stack-and-variables)
                                                    ((:s :stack :data) 'data)
                                                    ((:c :control) 'control)
                                                    ((:r :return) 'return))
                                  for variables = (cdr stack-and-variables)
                                  collect `(setf ,@(loop for variable in variables
                                                           for index from (1- (length variables)) downto 0
                                                           unless (eq variable '_)
                                                           append `(,variable (nth ,index ,stack)))
                                                 ,stack (concatenate 'list (remove nil (list ,@(nreverse mapping)))
                                                                     (subseq ,stack ,(length variables))))))
                       (return-state))))))
           
           (potentially-parse (?spec)
             (if (and (listp ?spec) 
                      (not (null ?spec)))
                 (parse-specs ?spec)
                 ?spec)))
    `(make-word ',name
      ,(parse-specs execute)
      ,(potentially-parse interpret)
      ,(potentially-parse compile))))

(defmacro defwords (&body forms)
  `(progn ,@(loop for form in forms collect `(defword ,@form))))

(defmacro state-λ (arguments &body body)
  (with-gensyms (result)
   `(lambda (state ,@arguments)
       (with-state state
        ;; Guaranteeing that the state is always returned
        (let ((,result (progn ,@body)))
          (if (statep ,result)
              ,result
              (return-state)))))))

(defwords
  (dup (:s a -- a a))
  (rot  (:s a b c -- b c a))
  (drop (:s _ --))
  (swap (:s a b -- b a))
  (over (:s a b -- a b a))
  (+ (:s a b -- (+ a b)))
  (* (:s a b -- (* a b)))
  (< (:s a b -- (flag (< a b))))
  (> (:s a b -- (flag (> a b))))
  (|.| (:s a -- (prog1 nil (print a))))
  (and (:s a b -- (logand a b)))
  (invert (:s a -- (lognot a)))
  (|0=| (:s a -- (reflag not a))))

;;;; Special words
(defwords
  (\; nil nil 
       (:c definition -- (prog1 (list :end-definition (second definition))
                            (add-definition definition)
                            (setf semantic-mode :interpret))))
  (|:| nil (:c -- (progn (setf semantic-mode :compile)
                         (state-λ (token)
                           (setf (car control) 
                                  (list :definition token nil))))))
  (immediate nil (:c -- (prog1 nil
                          (loop for π in control
                                when (and (listp π)
                                          (eq (car π) :end-definition))
                                  do (setf (.compile (word (second π)))
                                           :execute)))))
  ([ nil nil (:c -- (prog1 nil (setf semantic-mode :interpret))))
  (] nil (:c -- (prog1 nil (setf semantic-mode :compile))))
  (postpone nil nil (:c -- (state-λ (token)
                             (push (state-λ ()
                                     (assert (eq semantic-mode :execute))
                                     (funcall (.compile (word token)) state)) 
                                   (third (first control))))))
  (literal nil nil (:s a -- (prog1 nil
                              (push (state-λ ()
                                      (push a data))
                                    (third (first control)))))))


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
     (print state))
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
               (push token (third (first control)))
               (return-state))))
        (t (push token (third (first control)))
           (return-state))))))

(defun add-definition (definition)
  (print (list "add" definition))
  (destructuring-bind (_ name sentence) definition
    (setf sentence (reverse sentence))
    (make-word name
      (state-λ ()
        (print "enter")
        (let* ((previous-mode semantic-mode)
               (semantic-mode :execute)
               (new-state (run-code sentence (return-state))))
          (print "HI")
          (setf (state-semantic-mode new-state) previous-mode)
          new-state))
      :execute)))

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

