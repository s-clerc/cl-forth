(in-package :cl-forth)

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
                                                 ,stack (concatenate 'list 
                                                                     (remove nil (list ,@(nreverse mapping)))
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