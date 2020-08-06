(in-package :cl-forth)

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
       (:c definition -- (prog1 (list :end-definition (second definition))
                            (add-definition definition)
                            (setf semantic-mode :interpret))))
  
  (|:| nil ((setf semantic-mode :compile)
            (:c -- (state-λ (token) 
                     ;; Remove this function from stack
                     (pop control)
                     (push (list :definition token nil) control)))))
  
  (immediate nil (prog ((def-end (latest control :end-definition)))
                   (when def-end
                     (setf (.compile (word (second def-end)))
                           :execute))))
  
  ([ nil nil (setf semantic-mode :interpret))
  (] nil (setf semantic-mode :compile))
  
  (postpone nil nil (:c -- (state-λ (token)
                             (push (state-λ ()
                                     (assert (eq semantic-mode :execute))
                                     (funcall (.compile (word token)) state)) 
                                   (third (latest-definition control))))))
  (literal nil nil (:s a -- (prog1 nil
                              (push (state-λ ()
                                      (push a data))
                                    (third (latest-definition control)))))))