#include "templisp.h"

int main() {
  using env = InitMemory::env;
  using mem = InitMemory::memory;
  using ret1 = Eval<
    List<List<LAMBDA, List<X>, List<CONS, X, X>>, List<QUOTE, PIYO>>,
    env, mem>;
  std::cout << PrettyPrinter::print<ret1>() << std::endl;
  return 0;
}
