(defpackage cl-forth
  (:use :cl :cl-utilities))

(in-package :cl-forth)

(defparameter *dictionary* (make-hash-table))

(defmacro word (name)
  `(gethash ,name *dictionary*))

(defmacro wordq (name)
  `(word ',name))

(defclass word () 
  ((name        
     :initform nil
     :type symbolp
     :accessor .name)    
   (execute
     :initform #'identity
     :type functionp
     :accessor .execute)    
   (compile
     :initform #'identity
     :accessor .compile
     :type functionp)
   (immediatep
     :initform nil
     :accessor .immediatep)    
   ))

(defmacro define-word (options execute &optional (compile nil))
  (with-gensyms (result)  
    `(let ((,result (make-instance 'word)))
       ,(if (symbolp options)
            `(setf (.name ,result) ',options)
            `(progn 
               (setf (.name ,result) ',(first options))
               (when (> 1 (length ,options))
                 (setf (.immediatep ,result) ',(second options)))))
       ,(when execute
          `(setf (.execute ,result) ,execute))
       ,(case compile 
          (:same `(setf (.compile ,result) (.execute ,result)))
          ((:default nil) nil)
          (t `(setf (.compile ,result) ,compile)))
       (setf (gethash (.name ,result) *dictionary*) ,result))))

(defun flag (boolean)
  "Lisp Boolean to Forth Flag"
  (if boolean -1 0))

(defun deflag (flag)
  "Forth flag to Lisp Boolean"
  (if (= flag 0) nil t))

;; Macro because `and` is not a function :(
(defmacro reflag (operator &rest arguments)
  "Converts arguments to lisp booleans, applies operator to them, and then
converts the result back to a flag"
  `(flag (,operator ,@(mapcar #'(lambda (argument) `(deflag ,argument)) arguments))))

(defmacro with-state (state &body body)
  `(destructuring-bind (data control return interpretingp) ,state
     ,@body))

(defmacro return-state ()
  '(values (list data control return interpretingp)))

(defmacro defword (options execute &optional (compile :same))
  (flet ((parse-specs (specs)
            ;; So if there is only one spec, only (:s ...) and not ((:s ...)) may be written
            (when (not (listp (first specs))) 
              (setf specs (list specs)))
            (let* ((specs (loop for spec in specs collect (split-sequence '-- spec)))
                   (variables (remove-duplicates (loop for spec in specs append (rest (first spec))))))
              (with-gensyms (state data control return interpretingp) 
                 `(lambda (,state) 
                    (destructuring-bind (,data ,control ,return ,interpretingp) ,state
                       (let (,@variables)
                         ,@(loop for (stack-and-variables mapping) in specs
                                 for stack = (case (first stack-and-variables)
                                                   ((:s :stack :data) data)
                                                   ((:c :control) control)
                                                   ((:r :return) return))
                                 for variables = (cdr stack-and-variables)
                                 collect `(setf ,@(loop for variable in variables
                                                        for index from (1- (length variables)) downto 0
                                                        append `((nth ,index ,stack) ,variable))
                                                ,stack (concatenate 'list (list ,@(nreverse mapping))
                                                                    (subseq ,stack ,(length variables))))))
                      (list ,data ,control ,return ,interpretingp)))))))
    `(define-word ,options
      ,(parse-specs execute)
      ,(if (listp compile)
           (parse-specs compile)
           compile))))

(defmacro defwords (&body forms)
  `(progn ,@(loop for form in forms collect `(defword ,@form))))

(define-word |:|
  (lambda (state)
    (with-state state
      (setf interpretingp nil)
      (push control ':unnamed-definition)
      (return-state)))
  nil)

(defwords
  (dup (:s a -- a a))
  (rot  (:s a b c -- b c a))
  (drop (:s a --))
  (swap (:s a b -- b a))
  (over (:s a b -- a b a))
  (+ (:s a b -- (+ a b)))
  (* (:s a b -- (* a b)))
  (< (:s a b -- (flag (< a b))))
  (> (:s a b -- (flag (> a b))))
  (|.| (:s a -- (progn (print a) nil)))
  (and (:s a b -- (logand a b)))
  (invert (:s a -- (lognot a)))
  (|0=| (:s a -- (reflag not a)))
  (|;| nil (:c definition -- )))
    

(defun run-code (tokens initial-state)
  (loop for token in tokens
        for state = initial-state 
            then (with-state state
                   (if interpretingp
                       (interpret-1 token state)
                       (compile-1 token state)))))

(defun interpret-1 (token state)
  "token state => state"
  (with-state state
    (cond ((word token) (.execute (word token))))))

(defun compile-1 (token state)
  "Compile one token"
  (with-state state
    ;;; This section will see if anything in the control stack requires
    ;;; immediate action
    (case (car control)
      (:unnamed-definition (setf control (cons (list :definition token) 
                                               (cdr control)))
                           (return-state)))
    
    ;;; This section will handle tokens in general
    (case token
      (|;| (destructuring-bind (definition . rest) control
              (assert (eq (car definition) :definition)
                      (definition))
              (add-definition definition)
              (setf control rest)
              (setf interpretingp t)
              (return-state))))))
