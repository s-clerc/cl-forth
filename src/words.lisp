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
  (immediate nil (prog ((def-end (latest control 'end-definition)))
                   (when def-end
                     (setf (.compile (word (second def-end)))
                           :execute))))
  
  (postpone nil nil (:c -- (state-λ (token)
                              ;; Remove the future function from the stack
                             (pop control)
                             (push (state-λ ()
                                     (assert (eq (car semantic-mode) :execute))
                                     (run-word-function state (word token) #'.compile)) 
                                   (def-sentence (latest-definition control))))))
  
  (literal nil nil ((:s a --)
                    (push (state-λ ()
                            (push a data))
                          (def-sentence (latest-definition control))))))

;; The following are segregated because they mess up parïnfer
(defwords 
  ([ nil nil (push :interpret semantic-mode))
  ;; From what I can tell, it is given in the spec that
  ;; `]` may only be used in compilation mode
  (] nil (pop semantic-mode)))

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
  (\; nil nil 
    ((pop semantic-mode)
     (:c definition -- (make-end-definition :name (second definition)))
     (add-definition definition)))
  (|:| ((push :compile semantic-mode)
        (:c -- (spec :c _ -- (make-definition
                                 ;; The token
                                 :name (car remaining-arguments)))))))

;;;; Control flow section
;;; The following data types are used for the sake of clarity
(define-list-structure origin
    (jump nil :type jump))

(define-list-structure destination
    (jump nil :type jump))

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

(defmacro jump-to-destination (control destination 
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
                  (jump-to-destination 
                      control 
                      begin-destination)))
  
  (while nil nil (:c begin-destination -- (create-origin control) begin-destination))
  
  (repeat nil nil ((:c while-origin begin-destination --)
                   ;; The order matters here
                   ;; We first want to put in the function to jump
                   ;;  back to destination
                   (jump-to-destination control begin-destination t)
                   ;; Then the jump "tag" so we can jump to the end of
                   ;; repeat if need be
                   (resolve-origin control while-origin "REPEAT·"))))

(define-list-structure loop
    (index 0 :type integer)
    (limit nil :type integer)
    (unloop-now nil :type boolean))


(defun perform-loop (internal-sentence state)
  (with-state state
    (let* ((loop (car return)))
      (loop for i from (loop-index loop) 
                  below (loop-limit loop)
            until (loop-unloop-now loop)
            do (setf (loop-index loop) i
                     state (run-code internal-sentence 
                                     state))
            finally (return (with-state state
                              (pop return)
                              (return-state)))))))

(defwords 
  (do ((:s limit initial --)
       (:r -- (make-loop
                  :index initial
                  :limit limit)))
      nil)
  (i ((:r loop -- loop)
      (:s -- (loop-index loop))))
  (loop nil nil (loop with sentence = (def-sentence (latest-definition control)) 
                      for token = (car sentence)
                      if (eq token 'do)
                        do (push (partial #'perform-loop (nreverse internal-sentence))
                                 sentence)
                           (setf (def-sentence (latest-definition control)) 
                                 sentence)
                        and return (return-state) 
                      else
                        collect (pop sentence) into internal-sentence)))