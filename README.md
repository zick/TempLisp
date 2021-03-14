# TempLisp
Lisp implementation in C++ templates.

## How to use
```
% ./compile.sh -e "(car '(a b c))" > car.cc
% g++ -std=c++14 -o car car.cc
% ./car
A
% ./compile.sh -e "
(defun fact (n) (if (eq n 0) 1 (* n (fact (- n 1)))))
(fact 10)" > fact10.cc
% g++ -std=c++14 -o fact10 fact10.cc
% ./fact10
FACT
3628800
% cat << EOF > gen.lisp
(defun gen (n) (lambda (m) (setq n (+ n m))))
(setq x (gen 100))
(x 10)
(x 90)
(x 300)
EOF
% ./compile.sh < gen.lisp > gen.cc
% g++ -std=c++14 -o gen gen.cc
% ./gen
GEN
<expr>
110
200
500
% cat gen.cc
#include "templisp.h"
int main() {
  using env = InitMemory::env;
  DEFINE_SYMBOL(GEN);
  DEFINE_SYMBOL(N);
  DEFINE_SYMBOL(M);
  DEFINE_SYMBOL(X);
  using ret0 = Eval<List<DEFUN, GEN, List<N>, List<LAMBDA, List<M>, List<SETQ, N, List<ADD, N, M>>>>, env, InitMemory::memory>;
  constexpr auto str0 = PrettyPrinter::Print<ret0>::toString();
  std::cout << str0 << std::endl;
  using ret1 = Eval<List<SETQ, X, List<GEN, Int<100>>>, env, ret0::memory>;
  constexpr auto str1 = PrettyPrinter::Print<ret1>::toString();
  std::cout << str1 << std::endl;
  using ret2 = Eval<List<X, Int<10>>, env, ret1::memory>;
  constexpr auto str2 = PrettyPrinter::Print<ret2>::toString();
  std::cout << str2 << std::endl;
  using ret3 = Eval<List<X, Int<90>>, env, ret2::memory>;
  constexpr auto str3 = PrettyPrinter::Print<ret3>::toString();
  std::cout << str3 << std::endl;
  using ret4 = Eval<List<X, Int<300>>, env, ret3::memory>;
  constexpr auto str4 = PrettyPrinter::Print<ret4>::toString();
  std::cout << str4 << std::endl;
  return 0;
}
% g++ -std=c++14 -S gen.cc
% tail -n 20 gen.s
        .section        __TEXT,__literal4,4byte_literals
L___const.main.str0:                    ## @__const.main.str0
        .asciz  "GEN"

        .section        __TEXT,__const
l___const.main.str1:                    ## @__const.main.str1
        .asciz  "<expr>"

        .section        __TEXT,__literal4,4byte_literals
L___const.main.str2:                    ## @__const.main.str2
        .asciz  "110"

L___const.main.str3:                    ## @__const.main.str3
        .asciz  "200"

L___const.main.str4:                    ## @__const.main.str4
        .asciz  "500"
```

## How does it work
All LISP objects are represented as C++ types.
Symbols are represented as empty structs.
Conses are repsented as structs which take 2 template parameters.
```c++
struct NIL {};
struct T {};
struct LAMBDA {};

template<typename T1, typename T2>
struct Cons {
  using car = T1;
  using cdr = T2;
};
```
LISP evaluator is also represented as C++ types.
Conditional expressions are represented by template specializations.
Eval *can be* represented as a struct which take 2 template parameters
if mutations are not necessary.
```c++
template<typename Exp, typename Env>
struct Eval {
  using value = typename Assoc<Exp, Env>::value::cdr;
};
template<int i, typename Env>
struct Eval<Int<i>, Env> {
  using value = Int<i>;
};
template<typename T1, typename T2, typename Env>
struct Eval<Cons<T1, T2>, Env> {
  using value = typename EvCom<T1, T2, Env>::value;
};
```
Actually TempLisp supports mutations.
To support mutations, *memory* is necessary.
The *memory* is something like a set of key/value pairs.
It's represented as an alist whose key is an Int and value is a immutable cons.
```c++
template<typename NextId, typename Alist>
struct Memory {
  using nextId = NextId;
  using alist = Alist;
};
```
Mutable conses are achieved by accessing conses via Memory.
```c++
template<typename Id>
struct ConsRef {
  using id = Id;
};
template<typename Car, typename Cdr, typename Mem>
struct MCons {
private:
  using currId = typename Mem::nextId;
  using nextId = Int<currId::value + 1>;
public:
  using value = ConsRef<currId>;
  using memory = Memory<
    nextId,
    Cons<Cons<currId, Cons<Car, Cdr>>,
         typename Mem::alist>>;
};
template<typename Ref, typename Mem>
struct MCarImpl {
  using value = typename Assoc<
    typename Ref::id,
    typename Mem::alist>>::value::cdr::car;
};
```
Now that Eval takes another parameter and returns another type for Memory.
```c++
template<typename Exp, typename Env, typename Mem>
struct Eval {
  using value = MCdr<typename FindVar<Exp, Env, Mem>::value, Mem>;
  using memory = Mem;
};
```
See templisp.h for details.

C++ types (representing LISP objects and evaluator) are resolved at compile time.
So **LISP expressions are evaluated at C++ compile time.**
