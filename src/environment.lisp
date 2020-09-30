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

(defmacro define-list-structure (options &rest parameters)
  (let* ((options (if (symbolp options)
                      (list options)
                      options))
         (symbol-name (symbol-name (car options)))
         (predicate (intern (concatenate 'string symbol-name
                                        (if (find #\- symbol-name)
                                            "-P"
                                            "P")))))
    `(defstruct (,@options
                  (:type list) 
                  :named
                  (:predicate ,predicate))
        ,@parameters)))

(define-list-structure state
  (data nil :type list)
  (control nil :type list)
  (return nil :type list)
  ;; elements are (member :interpret :compile :execute)
  (semantic-mode '(:interpret) :type list))

(defvar *state* (make-state))

(defun reset ()
  (setf *state* (make-state)))

(defmacro run (&body body)
  `(setf *state* (run-code ',body *state*)))

(defconstant +lambda-list+ '(data control return semantic-mode))

(defmacro with-state (state &body body)
  "Destructures the state struct, does not perform return manipulation"
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

(defmacro spec (&body specs)
  (flet ((split-specs (specs)
           (loop for spec in specs
                 with new-spec = nil
                 ;; If it does not start with a keyword then
                 ;; we assume it is not a stack-transformation
                 if (keywordp (car spec))
                   do (setf new-spec (split-sequence '-- spec)) and
                   collect new-spec into new-specs and
                   append (cdar new-spec) into variables
                 else
                   collect `((:prog) ,spec) into new-specs
                 finally 
                   (return (values new-specs (remove-duplicates variables)))))
         (determine-stack (indicator)
           (ccase indicator
             ((:s :stack :data) 'data)
             ((:c :control) 'control)
             ((:r :return) 'return)
             (:prog nil)))
         (stack-transformation (stack variables mapping)
           `(setf ,@(loop for variable in variables
                          for index from (1- (length variables)) downto 0
                          unless (eq variable '_)
                          append `(,variable (nth ,index ,stack)))
                ,stack (concatenate 'list 
                                    (remove nil (list ,@(nreverse mapping)))
                                    (subseq ,stack ,(length variables))))))
    ;; So if there is only one spec, only (:s ...) and not ((:s ...)) may be written
    (when (not (listp (first specs))) 
      (setf specs (list specs)))
    (multiple-value-bind (specs variables) (split-specs specs)
      (with-gensyms (state)
         `(lambda (,state &rest remaining-arguments) 
            (declare (ignorable remaining-arguments))
            (with-state ,state
              (let (,@(remove '_ variables))
                ,@(loop for (stack-and-variables mapping) in specs
                        for variables = (cdr stack-and-variables)
                        ;; We need to know if it's a stack-transformation (:s ...)
                        ;; or just some code which runs e.g. (progn ...)
                        for stack = (determine-stack (first stack-and-variables))
                        if (not (null stack))
                          collect (stack-transformation stack variables mapping)
                        else
                          collect mapping))
              (return-state)))))))

(defmacro defword (name execute &optional (interpret :execute) (compile nil))
  (flet ((potentially-parse (?spec)
           (if (and (listp ?spec) 
                    (not (null ?spec)))
               `(spec ,@?spec)
               ?spec)))
        `(make-word ',name
                    ,(potentially-parse execute)
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

(defun partial (func &rest args1)
  (lambda (&rest args2)
    (apply func (append args1 args2))))

(defmacro colon (name &body definition)
  `(run \: ,name ,@definition \;))