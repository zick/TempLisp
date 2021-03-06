#include "templisp.h"

DEFINE_SYMBOL(APPEND);
DEFINE_SYMBOL(A);
DEFINE_SYMBOL(B);
DEFINE_SYMBOL(C);
DEFINE_SYMBOL(D);
DEFINE_SYMBOL(E);
DEFINE_SYMBOL(F);

int main() {
  using ret1 = Eval<
    List<DEFUN, APPEND, List<X, Y>,
         List<IF, List<EQ, X, List<>>,
              Y,
              List<CONS, List<CAR, X>, List<APPEND, List<CDR, X>, Y>>>>,
    InitMemory::env, InitMemory::memory>;
  constexpr auto str1 = PrettyPrinter::Print<ret1>::toString();
  std::cout << str1 << std::endl;
  using ret2 = Eval<
    List<APPEND, List<QUOTE, List<A, B, C>>, List<QUOTE, List<D, E, F>>>,
    InitMemory::env, ret1::memory>;
  constexpr auto str2 = PrettyPrinter::Print<ret2>::toString();
  std::cout << str2 << std::endl;
  return 0;
}
