(defun conv1 (exp)
  (if (consp exp)
      (format nil "Cons<~A, ~A>" (conv1 (car exp)) (conv1 (cdr exp)))
      (format nil "~A" exp)))

(defun proper-list-p (obj)
  (cond ((null obj) t)
        ((consp obj) (proper-list-p (cdr obj)))
        (t nil)))

(defparameter
    *keywords*
  (list 'nil 't 'quote 'if 'lambda 'setq' defun 'car 'cdr 'cons 'atom 'eq 'add
        'mul 'sub 'div 'mod 'x 'y 'hoge 'fuga 'piyo))
(defparameter *symtab* nil)
(defun conv2 (exp)
  (cond ((proper-list-p exp)
         (format nil "List<~{~A~^, ~}>" (mapcar #'conv2 exp)))
        ((consp exp)
         (format nil "Cons<~A, ~A>" (conv2 (car exp)) (conv2 (cdr exp))))
        ((numberp exp) (format nil "Int<~A>" exp))
        ((eq exp '+) (format nil "ADD"))
        ((eq exp '*) (format nil "MUL"))
        ((eq exp '-) (format nil "SUB"))
        ((eq exp '/) (format nil "DIV"))
        (t (pushnew exp *symtab*)
           (format nil "~A" exp))))

(defun conv3 (exp)
  (let* ((*symtab* nil)
         (tpl (conv2 exp)))
    (loop for sym in (set-difference *symtab* *keywords*) do
         (format t "DEFINE_SYMBOL(~A);~%" sym))
    (format t "~A~%" tpl)))
