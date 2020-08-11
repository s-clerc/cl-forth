(in-package :cl-forth)

(defun flag (boolean)
  "Lisp Boolean to Forth Flag"
  (if boolean -1 0))

(defun deflag (flag)
  "Forth flag to Lisp Boolean"
  (not (= flag 0)))

;; Macro because `and` is not a function :(
(defmacro reflag (operator &rest arguments)
  "Converts arguments to lisp booleans, applies operator to them, and then
converts the result back to a flag"
  `(flag (,operator ,@(mapcar #'(lambda (argument) `(deflag ,argument)) arguments))))

(define-list-structure (end-definition (:conc-name end-def-))
  (name nil :type symbol))

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
  (|0=| (:s a -- (reflag not a)))
  ;;; Special words
  (|:| ((push :compile semantic-mode)
        (:c -- (spec :c _ -- (make-definition
                                 ;; The token
                                 :name (car remaining-arguments)))))) 
  
  (immediate nil (prog ((def-end (latest control 'end-definition)))
                   (when def-end
                     (setf (.compile (word (second def-end)))
                           :execute))))
  
  ([ nil nil (push :interpret semantic-mode))
  ;; From what I can tell, it is given in the spec that
  ;; `]` may only be used in compilation mode
  (] nil (pop semantic-mode))
  
  (postpone nil nil (:c -- (state-λ (token)
                              ;; Remove the future function from the stack
                             (pop control)
                             (push (state-λ ()
                                     (assert (eq (car semantic-mode) :execute))
                                     (run-word-function state (word token) #'.compile)) 
                                   (def-sentence (latest-definition control))))))
  
  (literal nil nil ((:s a -- )
                    (push (state-λ ()
                            (push a data))
                          (def-sentence (latest-definition control))))))     


(defun add-definition (definition)
  (print (list "add" definition))
  (destructuring-bind (_ name sentence) definition
    (setf sentence (coerce (reverse sentence) 'vector))
    (make-word name
      (state-λ ()
        (push :execute semantic-mode)
        (setf state (run-code sentence (return-state)))
        (pop (state-semantic-mode state))
        state)
      :execute)))

(defword \; nil nil 
    ((pop semantic-mode)
     (:c definition -- (make-end-definition :name (second definition)))
     (add-definition definition)))

(define-list-structure origin
    (jump nil :type jump))
(define-list-structure destination
    (jump nil :type jump))

;;; Control flow section
(defmacro create-origin (control 
                         &optional (condition '(not (deflag (pop data)))))
  (with-gensyms (origin)
    `(let ((,origin (make-origin
                          :jump (make-jump))))
       (push (state-λ ()
               (when ,condition
                 (push (origin-jump ,origin) semantic-mode)))
             (def-sentence (latest-definition ,control)))
       ,origin)))

(defmacro resolve-origin (control origin &optional (prefix "ORIG·"))
  `(prog1 nil
     (setf (jump-tag (origin-jump ,origin)) (gensym ,prefix))
     (push (origin-jump ,origin)
           (def-sentence (latest-definition ,control)))))

(defmacro create-destination (control &optional (prefix "DEST·"))
  (with-gensyms (jump)
    `(let ((,jump (make-jump
                      :tag (gensym ,prefix))))
       (push ,jump
             (def-sentence (latest-definition ,control)))
       (make-destination 
           :jump ,jump)))) 

(defmacro resolve-destination (control destination 
                               &optional (condition '(not (deflag (pop data)))))
  (with-gensyms ()
    `(push (state-λ ()
               (when ,condition
                 (push (destination-jump ,destination) semantic-mode)))
           (def-sentence (latest-definition ,control)))))
                                           
(defwords  
  (if nil nil (:c -- (create-origin control)))
  
  (then nil nil ((:c else-jump --)
                 (resolve-origin control else-jump "THEN·")))
                                
  (else nil nil ((:c if-jump -- (create-origin control t))
                 (resolve-origin control if-jump "ELSE·")))
  
  (begin nil nil (:c -- (create-destination control "BEGIN·")))
  (until nil nil ((:c begin-destination --)
                  (resolve-destination 
                      control 
                      begin-destination)))
