(in-package :cl-forth)

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
  (\; nil nil 
      (:c definition -- (prog1 (make-end-definition :name (second definition))
                          (add-definition definition)
                          (setf semantic-mode :interpret))))
  
  (|:| nil ((setf semantic-mode :compile)
            (:c -- (spec :c _ -- (make-definition
                                     ;; The token
                                     :name (car remaining-arguments)))))) 
  
  (immediate nil (prog ((def-end (latest control 'end-definition)))
                   (when def-end
                     (setf (.compile (word (second def-end)))
                           :execute))))
  
  ([ nil nil (setf semantic-mode :interpret))
  (] nil (setf semantic-mode :compile))
  
  (postpone nil nil (:c -- (state-λ (token)
                              ;; Remove the future function from the stack
                             (pop control)
                             (push (state-λ ()
                                     (assert (eq semantic-mode :execute))
                                     (run-word-function state (word token) #'.compile)) 
                                   (third (latest-definition control))))))
  
  (literal nil nil (:s a -- (prog1 nil
                              (push (state-λ ()
                                      (push a data))
                                    (third (latest-definition control)))))))

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