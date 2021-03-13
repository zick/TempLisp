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
        'mul 'sub 'div 'mod))
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
         (format t "  DEFINE_SYMBOL(~A);~%" sym))
    (format t "  using ret = Eval<~A, env, mem>;~%" tpl)))

(defun conv4 (exps)
  (let* ((*symtab* nil)
         (tpls (loop for exp in exps collect (conv2 exp))))
    (loop for sym in (set-difference *symtab* *keywords*) do
         (format t "  DEFINE_SYMBOL(~A);~%" sym))
    (loop for tpl in tpls and i = 0 then (1+ i) do
         (format t "  using ret~A = Eval<~A, env, ~A::memory>;~%"
                 i tpl (if (= i 0)
                           "InitMemory"
                           (format nil "ret~A" (1- i))))
         (format
          t
          "  constexpr auto str~A = PrettyPrinter::Print<ret~A>::toString();~%"
          i i)
         (format t "  std::cout << str~A << std::endl;~%" i))))


(let ((exps (loop for exp = (read t nil nil) then (read t nil nil)
                 while exp while exp collect exp)))
  (format t "#include \"templisp.h\"
int main() {
  using env = InitMemory::env;
")
  (conv4 exps)
  (format t "  return 0;
}
"))
