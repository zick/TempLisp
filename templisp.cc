#include "templisp.h"

int main() {
  using env = InitMemory::env;
  using mem = InitMemory::memory;
  using ret1 = Eval<
    List<DEFUN, PIYO, List<X>, List<LAMBDA, List<Y>, List<SETQ, X, List<ADD, X, Y>>>>,
    env, mem>;
  std::cout << PrettyPrinter::print<ret1>() << std::endl;
  using ret2 = Eval<
    List<SETQ, HOGE, List<PIYO, Int<100>>>,
    env, ret1::memory>;
  std::cout << PrettyPrinter::print<ret2>() << std::endl;
  using ret3 = Eval<List<HOGE, Int<10>>, env, ret2::memory>;
  std::cout << PrettyPrinter::print<ret3>() << std::endl;
  using ret4 = Eval<List<HOGE, Int<90>>, env, ret3::memory>;
  std::cout << PrettyPrinter::print<ret4>() << std::endl;
  using ret5 = Eval<List<HOGE, Int<300>>, env, ret4::memory>;
  std::cout << PrettyPrinter::print<ret5>() << std::endl;
  return 0;
}
