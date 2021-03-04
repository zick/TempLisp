#include <iostream>
#include <string>

const char* kLPar = "(";
const char* kRPar = ")";

#define DEFINE_SYMBOL(sym)                         \
  struct sym {                                     \
    static std::string toString() { return #sym; } \
  }

struct NIL {
  using car = NIL;
  using cdr = NIL;
  static std::string toString() { return "NIL"; }
};
DEFINE_SYMBOL(T);
DEFINE_SYMBOL(QUOTE);
DEFINE_SYMBOL(IF);
DEFINE_SYMBOL(LAMBDA);
DEFINE_SYMBOL(CAR);
DEFINE_SYMBOL(CDR);
DEFINE_SYMBOL(CONS);
DEFINE_SYMBOL(ATOM);
DEFINE_SYMBOL(EQ);
DEFINE_SYMBOL(ADD);
DEFINE_SYMBOL(MUL);
DEFINE_SYMBOL(SUB);
DEFINE_SYMBOL(DIV);
DEFINE_SYMBOL(MOD);

DEFINE_SYMBOL(X);
DEFINE_SYMBOL(Y);
DEFINE_SYMBOL(HOGE);
DEFINE_SYMBOL(FUGA);
DEFINE_SYMBOL(PIYO);

template<int i>
struct Int {
  static constexpr int value = i;
  static std::string toString() { return std::to_string(value); }
};

template <typename T1, typename T2>
struct Add {
  using value = Int<T1::value + T2::value>;
};
template <typename T1, typename T2>
struct Mul {
  using value = Int<T1::value * T2::value>;
};
template <typename T1, typename T2>
struct Sub {
  using value = Int<T1::value - T2::value>;
};
template <typename T1, typename T2>
struct Div {
  using value = Int<T1::value / T2::value>;
};
template <typename T1, typename T2>
struct Mod {
  using value = Int<T1::value % T2::value>;
};

template<typename T1> struct isCons;

template<typename T1, typename T2>
struct Cons {
  using car = T1;
  using cdr = T2;

  template<
    typename T3 = T2,
    typename std::enable_if_t<std::is_same<T3, NIL>::value>* = nullptr>
  static std::string toString(const std::string& prefix=kLPar) {
    return prefix + T1::toString() + kRPar;
  }
  template<
    typename T3 = T2,
    typename std::enable_if_t<isCons<T3>::value>* = nullptr>
  static std::string toString(const std::string& prefix=kLPar) {
    return prefix + T1::toString() + T3::toString(" ");
  }
  template<
    typename T3 = T2,
    typename std::enable_if_t<
      !std::is_same<T3, NIL>::value &&
      !isCons<T3>::value>* = nullptr>
  static std::string toString(const std::string& prefix=kLPar) {
    return prefix + T1::toString() + " . " + T2::toString() + kRPar;
  }
};

template<typename T1>
struct isCons : std::false_type {};
template<typename T1, typename T2>
struct isCons<Cons<T1, T2>> : std::true_type {};

template<typename... Tail> struct ListImpl;
template<>
struct ListImpl<> {
  using value = NIL;
};
template<typename Head, typename... Tail>
struct ListImpl<Head, Tail...> {
  using value = Cons<Head, typename ListImpl<Tail...>::value>;
};
template<typename... Args>
using List = typename ListImpl<Args...>::value;

template<typename T1>
struct Atom {
  using value = T;
};
template<typename T1, typename T2>
struct Atom<Cons<T1, T2>> {
  using value = NIL;
};

template <typename T1, typename T2>
struct Eq {
  using value = NIL;
};
template <typename T1>
struct Eq<T1, T1> {
  using value = T;
};

template<typename Key, typename Alist, typename Cond = std::false_type,
         typename Ret = NIL>
struct Assoc {
  using value = typename Assoc<
    Key, typename Alist::cdr,
    typename std::is_same<typename Alist::car::car, Key>::type,
    typename Alist::car>::value;
};
template<typename Key, typename Alist, typename Ret>
struct Assoc<Key, Alist, std::true_type, Ret> {
  using value = Ret;
};
template<typename Key, typename Ret>
struct Assoc<Key, NIL, std::false_type, Ret> {
  using value = NIL;
};

template<typename Sym>
struct Subr {
  using name = Sym;
  static std::string toString() { return "<subr " + Sym::toString() + ">"; }
};
template<typename Arg>
struct SubrCar {
  using value = typename Arg::car::car;
};
template<typename Arg>
struct SubrCdr {
  using value = typename Arg::car::cdr;
};
template<typename Arg>
struct SubrCons {
  using value = Cons<typename Arg::car, typename Arg::cdr::car>;
};
template<typename Arg>
struct SubrAtom {
  using value = typename Atom<typename Arg::car>::value;
};
template<typename Arg>
struct SubrEq {
  using value = typename Eq<typename Arg::car, typename Arg::cdr::car>::value;
};
template<typename Arg>
struct SubrAdd {
  using value = typename Add<typename Arg::car, typename Arg::cdr::car>::value;
};
template<typename Arg>
struct SubrMul {
  using value = typename Mul<typename Arg::car, typename Arg::cdr::car>::value;
};
template<typename Arg>
struct SubrSub {
  using value = typename Sub<typename Arg::car, typename Arg::cdr::car>::value;
};
template<typename Arg>
struct SubrDiv {
  using value = typename Div<typename Arg::car, typename Arg::cdr::car>::value;
};
template<typename Arg>
struct SubrMod {
  using value = typename Mod<typename Arg::car, typename Arg::cdr::car>::value;
};

template<typename Arg, typename Body>
struct Expr {
  static std::string toString() { return "<expr>"; }
};

template<typename Fn, typename Arg, typename Env>
struct EvCom;

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

template<typename Lst, typename Env>
struct Evlis {
  using value = Cons<typename Eval<typename Lst::car, Env>::value,
                     typename Evlis<typename Lst::cdr, Env>::value>;
};
template<typename Env>
struct Evlis<NIL, Env> {
  using value = NIL;
};

template<typename L1, typename L2, typename Rest = NIL>
struct Pairlis {
  using value = Cons<
    Cons<typename L1::car, typename L2::car>,
    typename Pairlis<typename L1::cdr, typename L2::cdr, Rest>::value>;
};
template<typename L2, typename Rest>
struct Pairlis<NIL, L2, Rest> {
  using value = Rest;
};

// Apply2: Fn and Arg have been already evaluated.
template<typename Fn, typename Arg, typename Env>
struct Apply2 {
  using value = NIL;
};
template<typename Par, typename Body, typename Arg, typename Env>
struct Apply2<Expr<Par, Body>, Arg, Env> {
  using value = typename Eval<
    typename Body::car,
    typename Pairlis<Par, Arg, Env>::value>::value;
};
template<typename Arg, typename Env>
struct Apply2<Subr<CAR>, Arg, Env> {
  using value = typename SubrCar<Arg>::value;
};
template<typename Arg, typename Env>
struct Apply2<Subr<CDR>, Arg, Env> {
  using value = typename SubrCdr<Arg>::value;
};
template<typename Arg, typename Env>
struct Apply2<Subr<CONS>, Arg, Env> {
  using value = typename SubrCons<Arg>::value;
};
template<typename Arg, typename Env>
struct Apply2<Subr<EQ>, Arg, Env> {
  using value = typename SubrEq<Arg>::value;
};
template<typename Arg, typename Env>
struct Apply2<Subr<ATOM>, Arg, Env> {
  using value = typename SubrAtom<Arg>::value;
};
template<typename Arg, typename Env>
struct Apply2<Subr<ADD>, Arg, Env> {
  using value = typename SubrAdd<Arg>::value;
};
template<typename Arg, typename Env>
struct Apply2<Subr<MUL>, Arg, Env> {
  using value = typename SubrMul<Arg>::value;
};
template<typename Arg, typename Env>
struct Apply2<Subr<SUB>, Arg, Env> {
  using value = typename SubrSub<Arg>::value;
};
template<typename Arg, typename Env>
struct Apply2<Subr<DIV>, Arg, Env> {
  using value = typename SubrDiv<Arg>::value;
};
template<typename Arg, typename Env>
struct Apply2<Subr<MOD>, Arg, Env> {
  using value = typename SubrMod<Arg>::value;
};

// Apply: Arg has been already evaluated but Fn hasn't yet.
template<typename Fn, typename Arg, typename Env>
struct Apply {
  using value =
    typename Apply2<typename Eval<Fn, Env>::value, Arg, Env>::value;
};
template<typename Par, typename Body, typename Arg, typename Env>
struct Apply<Cons<LAMBDA, Cons<Par, Body>>, Arg, Env> {
  using value = typename Eval<
    typename Body::car,
    typename Pairlis<Par, Arg, Env>::value>::value;
};

template<typename Cond, typename Then, typename Else>
struct If {
  using value = Then;
};
template<typename Then, typename Else>
struct If<NIL, Then, Else> {
  using value = Else;
};

template<typename Fn, typename Arg, typename Env>
struct EvCom {
  using value =
    typename Apply<Fn, typename Evlis<Arg, Env>::value, Env>::value;
};
template<typename Arg, typename Env>
struct EvCom<QUOTE, Arg, Env> {
  using value = typename Arg::car;
};
template<typename Arg, typename Env>
struct EvCom<IF, Arg, Env> {
  using value =
    typename Eval<
      typename If<typename Eval<typename Arg::car, Env>::value,
         typename Arg::cdr::car,
         typename Arg::cdr::cdr::car>::value,
      Env>::value;
};
template<typename Arg, typename Env>
struct EvCom<LAMBDA, Arg, Env> {
  using value = Expr<typename Arg::car, typename Arg::cdr>;
};

using G_ENV = List<
  Cons<T, T>, Cons<NIL, NIL>, Cons<CAR, Subr<CAR>>, Cons<CDR, Subr<CDR>>,
  Cons<CONS, Subr<CONS>>, Cons<EQ, Subr<EQ>>, Cons<ATOM, Subr<ATOM>>,
  Cons<ADD, Subr<ADD>>, Cons<MUL, Subr<MUL>>, Cons<SUB, Subr<SUB>>,
  Cons<DIV, Subr<DIV>>, Cons<MOD, Subr<MOD>>>;
