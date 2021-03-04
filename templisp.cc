#include "templisp.h"

int main() {
  std::cout << Eval<List<List<LAMBDA, List<X, Y>, List<IF, List<EQ, Y, Int<0>>, Int<1>, List<MUL, Y, List<X, X, List<SUB, Y, Int<1>>>>>>, List<LAMBDA, List<X, Y>, List<IF, List<EQ, Y, Int<0>>, Int<1>, List<MUL, Y, List<X, X, List<SUB, Y, Int<1>>>>>>, Int<10>>, G_ENV>::value::value << std::endl;
  return 0;
}
