(* Real programs consist of many components. *)
(* Specify the dependencies among these components *)
(* using signatures and functors. *)

fun eq_int (n:int, m:int)
  = (n = m)

fun is_zero (n)
  = eq_int(n,0)

exception Too_small

(* better name predecessor *)
fun pred (n)
  = if eq_int(n,0)
    then raise Too_small
    else n - 1

(* better name successor *)
fun succ (n)
  = n + 1

fun plus (n,m)
  = if is_zero(n)
    then m
    else succ(plus(pred(n),m))

datatype num =
         Zero
         | One_more_than of num

fun is_zero (Zero)
  = true
  | is_zero (not_zero)
    = false

fun pred (Zero)
  = raise Too_small
  | pred (One_more_than(n))
    = n

fun succ (n)
  = One_more_than(n)

fun plus (n,m)
  = if is_zero(n)
    then m
    else succ(plus(pred(n),m))
(* the two definitions of plus are identical *)

(* here is a way to write down these minimal requirements *)
(* for our building blocks. *)
signature N =(* a better name for this signature would be NUMBERS_BY_PEANO. *)
sig
    type number
    exception Too_small
    val succ : number -> number
    val pred : number -> number
    val is_zero : number -> bool
end

functor NumberAsNum() :> N =
struct
datatype num = Zero
             | One_more_than of num
type number = num
exception Too_small
fun succ (n)
  = One_more_than(n)
fun pred (Zero)
  = raise Too_small
  | pred (One_more_than(n))
    = n
fun is_zero (Zero)
  = true
  | is_zero (a_num)
    = false
end

(* the result of using the functor is a structure with signature N *)
functor NumberAsInt() :> N =
struct
type number = int
exception Too_small
fun succ (n)
  = n + 1
fun pred (n)
  = if eq_int(n,0)
    then raise Too_small
    else n - 1
fun is_zero (n)
  = eq_int(n,0)
end

(* let's use a functor to build a structure *)
structure IntStruct =
NumberAsInt()

structure NumStruct =
NumberAsNum()

signature P = (* a better name: PLUS_OVER_NUMBER.*)
sig
    type number
    val plus : (number * number) -> number
end

functor PON(structure a_N : N)
        :>
        P where type number = a_N.number
                              =
                              struct
                              type number = a_N.number
                              fun plus (n,m)
                                = if a_N.is_zero(n)
                                  then m
                                  else a_N.succ(plus(a_N.pred(n),m))
                              end

structure IntArith =
PON(structure a_N = IntStruct)

structure NumArith =
PON(structure a_N = NumStruct)

(* better name for this would be NUMBERS_WITH_CONCEAL_REVEAL. *)
signature N_C_R =
sig
    type number
    exception Too_small
    val conceal : int -> number
    val succ : number -> number
    val pred : number -> number
    val is_zero : number -> bool
    val reveal : number -> int
end

functor NumberAsInt() :> N_C_R =
struct
type number = int
exception Too_small
fun conceal(n)
  = n
fun succ(n)
  = n + 1
fun pred(n)
  = if eq_int(n,0)
    then raise Too_small
    else n - 1
fun is_zero(n)
  = eq_int(n,0)
fun reveal(n)
  = n
end

functor NumberAsNum() :> N_C_R =
struct
datatype num = Zero
             | One_more_than of num
type number = num
exception Too_small
fun conceal(n)
  = if eq_int(n,0)
    then Zero
    else One_more_than(conceal(n-1))
fun succ(n)
  = One_more_than(n)
fun pred(Zero)
  = raise Too_small
  | pred (One_more_than(n))
    = n
fun is_zero(Zero)
  = true
  | is_zero (a_num)
    = false
fun reveal(n)
  = if is_zero(n)
    then 0
    else 1 + reveal(pred(n))
end

structure IntStruct =
NumberAsInt()

structure NumStruct =
NumberAsNum()

structure IntArith =
PON(structure a_N = IntStruct)

structure NumArith =
PON(structure a_N = NumStruct)

functor NumberAsInt2()
        :>
        N where type number = int
                              =
                              struct
                              type number = int
                              exception Too_small
                              fun succ(n)
                                = n + 1
                              fun pred(n)
                                = if eq_int(n,0)
                                  then raise Too_small
                                  else n - 1
                              fun is_zero(n)
                                = eq_int(n,0)
                              end

structure IntStruct2 =
NumberAsInt2()

structure IntArith2 =
PON(structure a_N = IntStruct2)

(* functor NumberAsNum2() *)
(*         :> *)
(*         N where type number = num *)
(*                               = *)
(*                               struct *)
(*                               datatype num = *)
(*                                        Zero *)
(*                                        | One_more_than of num *)
(*                               type number = num *)
(*                               exception Too_small *)
(*                               fun succ(n) *)
(*                                 = One_more_than(n) *)
(*                               fun pred(Zero) *)
(*                                 = raise Too_small *)
(*                                 | pred(One_more_than(n)) *)
(*                                   = n *)
(*                               fun is_zero(Zero) *)
(*                                 = true *)
(*                                 | is_zero (a_num) *)
(*                                   = false *)
(*                               end *)

(* structure NumStruct2 = *)
(* NumberAsNum2() *)

(* structure NumArith2 = *)
(* PON(structure a_N = NumStruct2) *)

signature S =
sig
    type number1
    type number2
    val similar : (number1 * number2) -> bool
end

functor Same(structure a_N : N
             structure b_N : N)
        :>
        S where type number1 = a_N.number
                                   where type number2 = b_N.number
                                                        =
                                                        struct
                                                        type number1 = a_N.number
                                                        type number2 = b_N.number
                                                        fun sim (n,m)
                                                          = if a_N.is_zero(n)
                                                            then b_N.is_zero(m)
                                                            else sim(a_N.pred(n),
                                                                     b_N.pred(m))
                                                        fun similar (n,m)
                                                          = ((sim(n,m)
                                                              handle
                                                              a_N.Too_small => false)
                                                             handle
                                                             b_N.Too_small => false)
                                                        end

structure SimIntNum =
Same(structure a_N = IntStruct
     structure b_N = NumStruct)

structure SimNumInt =
Same(structure a_N = NumStruct
     structure b_N = IntStruct)
(* are functors like functions *)
(* yes, but they only consume and produce structures, not values. *)

structure SimNumNum =
Same(structure a_N = NumStruct
     structure b_N = NumStruct)

fun new_plus(x,y)
  = NumStruct.reveal(
      NumArith.plus(
          NumStruct.conceal(x),
          NumStruct.conceal(y)))

fun new_plus2(x,y)
  = IntStruct.reveal(
      IntArith.plus(
          IntStruct.conceal(x),
          IntStruct.conceal(y)))

signature J =
sig
    val new_plus : (int * int) -> int
end

(* we use sharing type to specify that the two type *)
(* must be the same. *)
functor NP (structure a_N : N_C_R
            structure a_P : P
            sharing type
            a_N.number
            =
            a_P.number)
        :>
        J
=
struct
fun new_plus(x,y)
  = a_N.reveal(
      a_P.plus(
          a_N.conceal(x),
          a_N.conceal(y)))
end

(* a better name for NP is NewPlusFunctor *)

structure NPStruct =
NP(structure a_N = NumStruct
   structure a_P = NumArith)

(* say NPStruct in one expression. *)
structure NPOStruct =
NP(structure a_N = NumberAsNum()
   structure a_P =
   PON(structure a_N = a_N))

(* can we build all programs in one expression? *)
(* yes *)

(* a better name for this signature would be TIMES_OVER_NUMBER *)
signature T = sig
    type number
    val times :
        (number * number) -> number
end

functor TON(structure a_N : N
            structure a_P : P
            sharing type
            a_N.number
            =
            a_P.number)
        :>
        T where type number = a_N.number
                              =
                              struct
                              type number = a_N.number
                              fun times (n,m)
                                = if a_N.is_zero(m)
                                  then m
                                  else a_P.plus(n,
                                                times(n,a_N.pred(m)))
                              end
