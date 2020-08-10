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

(defword \; nil nil 
    ((pop semantic-mode)
     (:c definition -- (make-end-definition :name (second definition)))
     (add-definition definition)))

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

(defwords
  (if nil nil (:c -- (let ((jump (make-jump)))
                       (push (state-λ ()
                               (unless (deflag (pop data))
                                  (push jump control))) 
                             (def-sentence (latest-definition control)))
                       jump)))
  (then nil nil (:c if-jump -- (prog1 nil
                                 (setf (jump-tag if-jump) (gensym "THEN"))
                                 (push if-jump (def-sentence (latest-definition control))))))
                                
  (else nil nil (:c if-jump -- (let ((jump (make-jump)))
                                (push (state-λ ()
                                        (push jump control)) 
                                      (def-sentence (latest-definition control)))
                                (setf (jump-tag if-jump) (gensym "ELSE"))
                                (push if-jump
                                      (def-sentence (latest-definition control)))
                                jump))))



