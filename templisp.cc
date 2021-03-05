#include "templisp.h"

int main() {
  using env = InitMemory::env;
  using mem = InitMemory::memory;
  using result =
    Eval<List<List<LAMBDA, List<X, Y>, List<IF, List<EQ, Y, Int<0>>, Int<1>, List<MUL, Y, List<X, X, List<SUB, Y, Int<1>>>>>>, List<LAMBDA, List<X, Y>, List<IF, List<EQ, Y, Int<0>>, Int<1>, List<MUL, Y, List<X, X, List<SUB, Y, Int<1>>>>>>, Int<10>>, env, mem>;
  std::cout << PrettyPrinter::print<result>() << std::endl;
  std::cout << result::memory::toString() << std::endl;
  return 0;
}
