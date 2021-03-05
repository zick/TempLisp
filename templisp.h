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

template<typename NextId, typename Alist>
struct Memory {
  using nextId = NextId;
  using alist = Alist;
  static std::string toString() {
    return "[next: " + NextId::toString() +
      ", alist: " + Alist::toString() + "]";
  }
};
template<typename Mem>
using MemA = typename Mem::alist;
template<typename T1>
using Mem_t = typename T1::memory;

template<typename Id>
struct ConsRef {
  using id = Id;
  static std::string toString() { return "<ref " + Id::toString() + ">"; }
};

template<typename Ref, typename Mem>
struct MCarImpl {
  using value = typename Assoc<typename Ref::id, MemA<Mem>>::value::cdr::car;
};
template<typename T1, typename T2, typename Mem>
struct MCarImpl<Cons<T1, T2>, Mem> {
  using value = T1;
};
template<typename Mem>
struct MCarImpl<NIL, Mem> {
  using value = NIL;
};
template<typename Ref, typename Mem>
using MCar = typename MCarImpl<Ref, Mem>::value;
template<typename Ref, typename Mem>
struct MCdrImpl {
  using value = typename Assoc<typename Ref::id, MemA<Mem>>::value::cdr::cdr;
};
template<typename T1, typename T2, typename Mem>
struct MCdrImpl<Cons<T1, T2>, Mem> {
  using value = T2;
};
template<typename Mem>
struct MCdrImpl<NIL, Mem> {
  using value = NIL;
};
template<typename Ref, typename Mem>
using MCdr = typename MCdrImpl<Ref, Mem>::value;
template<typename Car, typename Cdr, typename Mem>
struct MCons {
private:
  using currId = typename Mem::nextId;
  using nextId = Int<currId::value + 1>;
public:
  using value = ConsRef<currId>;
  using memory = Memory<nextId, Cons<Cons<currId, Cons<Car, Cdr>>, MemA<Mem>>>;
};

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

template<typename Mem, typename... Tail> struct MList;
template<typename Mem>
struct MList<Mem> {
  using value = NIL;
  using memory = Mem;
};
template<typename Mem, typename Head, typename... Tail>
struct MList<Mem, Head, Tail...> {
private:
  using tail = MList<Mem, Tail...>;
  using head = MCons<Head, typename tail::value, typename tail::memory>;
public:
  using value = typename head::value;
  using memory =typename  head::memory;
};

template<typename Ref, typename Val, typename Mem>
struct SetCar {
private:
  using currId = typename Mem::nextId;
  using cdr = MCdr<Ref, Mem>;
public:
  using value = Val;
  using memory =
    Memory<currId, Cons<Cons<typename Ref::id, Cons<Val, cdr>>, MemA<Mem>>>;
};
template<typename Ref, typename Val, typename Mem>
struct SetCdr {
private:
  using currId = typename Mem::nextId;
  using car = MCar<Ref, Mem>;
public:
  using value = Val;
  using memory =
    Memory<currId, Cons<Cons<typename Ref::id, Cons<car, Val>>, MemA<Mem>>>;
};

template<typename Key, typename Alist, typename Mem,
         typename Cond = std::false_type, typename Ret = NIL>
struct MAssoc {
  using value = typename MAssoc<
    Key, MCdr<Alist, Mem>, Mem,
    typename std::is_same<MCar<MCar<Alist, Mem>, Mem>, Key>::type,
    MCar<Alist, Mem>>::value;
};
template<typename Key, typename Alist, typename Mem, typename Ret>
struct MAssoc<Key, Alist, Mem, std::true_type, Ret> {
  using value = Ret;
};
template<typename Key, typename Mem, typename Ret>
struct MAssoc<Key, NIL, Mem, std::false_type, Ret> {
  using value = NIL;
};

template<typename Key, typename Env, typename Mem, typename Ret = NIL>
struct FindVar {
  using value = typename FindVar<
    Key, MCdr<Env, Mem>, Mem,
    typename MAssoc<Key, MCar<Env, Mem>, Mem>::value>::value;
};
template<typename Key, typename Env, typename Mem, typename Id>
struct FindVar<Key, Env, Mem, ConsRef<Id>> {
  using value = ConsRef<Id>;
};
template<typename Key, typename Mem>
struct FindVar<Key, NIL, Mem, NIL> {
  using value = NIL;
};

template<typename Sym, typename Val, typename Env, typename Mem>
struct AddToEnv {
private:
  using bind = MCons<Sym, Val, Mem>;
  using alist = MCons<typename bind::value,
                      MCar<Env, typename bind::memory>,
                      typename bind::memory>;
public:
  using memory = typename
    SetCar<Env, typename alist::value, typename alist::memory>::memory;
};

template<typename T1>
struct Atom {
  using value = T;
};
template<typename T1, typename T2>
struct Atom<Cons<T1, T2>> {
  using value = NIL;
};
template<typename Id>
struct Atom<ConsRef<Id>> {
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

template<typename Sym>
struct Subr {
  using name = Sym;
  static std::string toString() { return "<subr " + Sym::toString() + ">"; }
};
template<typename Arg, typename Mem>
struct SubrCar {
  using value = MCar<MCar<Arg, Mem>, Mem>;
  using memory = Mem;
};
template<typename Arg, typename Mem>
struct SubrCdr {
  using value = MCdr<MCar<Arg, Mem>, Mem>;
  using memory = Mem;
};
template<typename Arg, typename Mem>
struct SubrCons {
private:
  using result = MCons<MCar<Arg, Mem>, MCar<MCdr<Arg, Mem>, Mem>, Mem>;
public:
  using value = typename result::value;
  using memory = typename result::memory;
};
template<typename Arg, typename Mem>
struct SubrAtom {
  using value = typename Atom<MCar<Arg, Mem>>::value;
  using memory = Mem;
};
template<typename Arg, typename Mem>
struct SubrEq {
  using value = typename Eq<MCar<Arg, Mem>, MCar<MCdr<Arg, Mem>, Mem>>::value;
  using memory = Mem;
};
template<typename Arg, typename Mem>
struct SubrAdd {
  using value = typename Add<MCar<Arg, Mem>, MCar<MCdr<Arg, Mem>, Mem>>::value;
  using memory = Mem;
};
template<typename Arg, typename Mem>
struct SubrMul {
  using value = typename Mul<MCar<Arg, Mem>, MCar<MCdr<Arg, Mem>, Mem>>::value;
  using memory = Mem;
};
template<typename Arg, typename Mem>
struct SubrSub {
  using value = typename Sub<MCar<Arg, Mem>, MCar<MCdr<Arg, Mem>, Mem>>::value;
  using memory = Mem;
};
template<typename Arg, typename Mem>
struct SubrDiv {
  using value = typename Div<MCar<Arg, Mem>, MCar<MCdr<Arg, Mem>, Mem>>::value;
  using memory = Mem;
};
template<typename Arg, typename Mem>
struct SubrMod {
  using value = typename Mod<MCar<Arg, Mem>, MCar<MCdr<Arg, Mem>, Mem>>::value;
  using memory = Mem;
};

template<typename Arg, typename Body, typename Env>
struct Expr {
  static std::string toString() { return "<expr>"; }
};

template<typename Fn, typename Arg, typename Env, typename Mem>
struct EvCom;

template<typename Exp, typename Env, typename Mem>
struct Eval {
  using value = MCdr<typename FindVar<Exp, Env, Mem>::value, Mem>;
  using memory = Mem;
};
template<int i, typename Env, typename Mem>
struct Eval<Int<i>, Env, Mem> {
  using value = Int<i>;
  using memory = Mem;
};
template<typename T1, typename T2, typename Env, typename Mem>
struct Eval<Cons<T1, T2>, Env, Mem> {
private:
  using result = EvCom<T1, T2, Env, Mem>;
public:
  using value = typename result::value;
  using memory = typename result::memory;
};

template<typename Lst, typename Env, typename Mem>
struct Evlis {
private:
  using x = Eval<MCar<Lst, Mem>, Env, Mem>;
  using xs = Evlis<MCdr<Lst, typename x::memory>, Env, typename x::memory>;
  using result = MCons<typename x::value, typename xs::value,
                       typename xs::memory>;
public:
  using value = typename result::value;
  using memory = typename result::memory;
};
template<typename Env, typename Mem>
struct Evlis<NIL, Env, Mem> {
  using value = NIL;
  using memory = Mem;
};

template<typename L1, typename L2, typename Mem, typename Rest = NIL>
struct Pairlis {
private:
  using x = MCons<MCar<L1, Mem>, MCar<L2, Mem>, Mem>;
  using mx = typename x::memory;
  using xs = Pairlis<MCdr<L1, mx>, MCdr<L2, mx>, mx, Rest>;
  using result = MCons<typename x::value, typename xs::value,
                       typename xs::memory>;
public:
  using value = typename result::value;
  using memory = typename result::memory;
};
template<typename L2, typename Mem, typename Rest>
struct Pairlis<NIL, L2, Mem, Rest> {
  using value = Rest;
  using memory = Mem;
};

// Apply2: Fn and Arg have been already evaluated.
template<typename Fn, typename Arg, typename Env, typename Mem>
struct Apply2 {
  using value = NIL;
  using memory = Mem;
};
template<typename Par, typename Body, typename EE,
         typename Arg, typename Env, typename Mem>
struct Apply2<Expr<Par, Body, EE>, Arg, Env, Mem> {
private:
  using binds = Pairlis<Par, Arg, Mem>;
  using env = MCons<typename binds::value, EE, Mem_t<binds>>;
  using result = Eval<MCar<Body, Mem_t<env>>,
                      typename env::value, typename env::memory>;
public:
  using value = typename result::value;
  using memory = typename result::memory;
};

#define REGISTER_SUBR(sym, fn) \
  template<typename Arg, typename Env, typename Mem>  \
  struct Apply2<Subr<sym>, Arg, Env, Mem> {           \
  private:                                            \
  using result = fn<Arg, Mem>;                        \
  public:                                             \
  using value = typename result::value;               \
  using memory = typename result::memory;             \
  };
REGISTER_SUBR(CAR, SubrCar);
REGISTER_SUBR(CDR, SubrCdr);
REGISTER_SUBR(CONS, SubrCons);
REGISTER_SUBR(EQ, SubrEq);
REGISTER_SUBR(ATOM, SubrAtom);
REGISTER_SUBR(ADD, SubrAdd);
REGISTER_SUBR(MUL, SubrMul);
REGISTER_SUBR(SUB, SubrSub);
REGISTER_SUBR(DIV, SubrDiv);
REGISTER_SUBR(MOD, SubrMod);

// Apply: Arg has been already evaluated but Fn hasn't yet.
template<typename Fn, typename Arg, typename Env, typename Mem>
struct Apply {
private:
  using fn = Eval<Fn, Env, Mem>;
  using result = Apply2<typename fn::value, Arg, Env, typename fn::memory>;
public:
  using value = typename result::value;
  using memory = typename result::memory;
};
template<typename Par, typename Body, typename Arg, typename Env, typename Mem>
struct Apply<Cons<LAMBDA, Cons<Par, Body>>, Arg, Env, Mem> {
private:
  using binds = Pairlis<Par, Arg, Mem>;
  using env = MCons<typename binds::value, Env, Mem_t<binds>>;
  using result = Eval<MCar<Body, Mem_t<env>>,
                      typename env::value, typename env::memory>;
public:
  using value = typename result::value;
  using memory = typename result::memory;
};

template<typename Cond, typename Then, typename Else>
struct If {
  using value = Then;
};
template<typename Then, typename Else>
struct If<NIL, Then, Else> {
  using value = Else;
};

template<typename Fn, typename Arg, typename Env, typename Mem>
struct EvCom {
private:
  using args = Evlis<Arg, Env, Mem>;
  using result = Apply<Fn, typename args::value, Env, typename args::memory>;
public:
  using value = typename result::value;
  using memory = typename result::memory;
};
template<typename Arg, typename Env, typename Mem>
struct EvCom<QUOTE, Arg, Env, Mem> {
  using value = MCar<Arg, Mem>;
  using memory = Mem;
};
template<typename Arg, typename Env, typename Mem>
struct EvCom<IF, Arg, Env, Mem> {
private:
  using cond = Eval<MCar<Arg, Mem>, Env, Mem>;
  using result = Eval<typename If<
    typename cond::value,
    MCar<MCdr<Arg, Mem_t<cond>>, Mem_t<cond>>,
    MCar<MCdr<MCdr<Arg, Mem_t<cond>>, Mem_t<cond>>, Mem_t<cond>>>::value,
                               Env, Mem_t<cond>>;
public:
  using value = typename result::value;
  using memory = typename result::memory;
};
template<typename Arg, typename Env, typename Mem>
struct EvCom<LAMBDA, Arg, Env, Mem> {
  using value = Expr<MCar<Arg, Mem>, MCdr<Arg, Mem>, Env>;
  using memory = Mem;
};

struct InitMemory {
private:
  using mem0 = Memory<Int<0>, NIL>;
  using s0 = MCons<NIL, NIL, mem0>;
public:
  using env = s0::value;
private:
  using s1 = AddToEnv<NIL, NIL, env, s0::memory>;
  using s2 = AddToEnv<T, T, env, s1::memory>;
  using s3 = AddToEnv<CAR, Subr<CAR>, env, s2::memory>;
  using s4 = AddToEnv<CDR, Subr<CDR>, env, s3::memory>;
  using s5 = AddToEnv<CONS, Subr<CONS>, env, s4::memory>;
  using s6 = AddToEnv<EQ, Subr<EQ>, env, s5::memory>;
  using s7 = AddToEnv<ATOM, Subr<ATOM>, env, s6::memory>;
  using s8 = AddToEnv<ADD, Subr<ADD>, env, s7::memory>;
  using s9 = AddToEnv<MUL, Subr<MUL>, env, s8::memory>;
  using s10 = AddToEnv<SUB, Subr<SUB>, env, s9::memory>;
  using s11 = AddToEnv<DIV, Subr<DIV>, env, s10::memory>;
  using s12 = AddToEnv<MOD, Subr<MOD>, env, s11::memory>;
public:
  using memory = s12::memory;
};
