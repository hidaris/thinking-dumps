(* Some functions consume values of arow type; some
   produce values of arrow type. *)

fun identity (x) =
  x

fun true_maker (x) =
  true

datatype bool_or_int = Hot of bool
                     | Cold of int

fun hot_maker (x) =
  Hot
(* constructors are functions *)

fun help (f) =
  Hot(
    true_maker(
      if true_maker(5)
      then f
      else true_maker))

(* consists of an int and a funtion that consums that
   values to produce the next chain. *)
datatype chain =
         Link of (int * (int -> chain))

fun ints (n) =
  Link(n + 1,ints)

fun skips (n) =
  Link(n + 2,skips)

fun eq_int (n:int,m:int) = (n = m)

fun divides_evenly (n,c) =
  eq_int((n mod c),0)

fun is_mod_5_or_7 (n) =
  if divides_evenly(n,5)
  then true
  else divides_evenly(n,7)

fun some_ints (n) =
  if is_mod_5_or_7(n + 1)
  then Link(n + 1,some_ints)
  else some_ints(n + 1)

fun chain_item (n,Link(i,f)) =
  if eq_int(n,1)
  then i
  else chain_item(n-1,f(i))

(* Prime: a number is prime if it is (strictly) greater
   than 1 and can be divided evenly only by itself and 1.*)

(* local *)
(*     fun has_no_divisors (n,c) *)
(*       = if eq_int(c,1) *)
(*         then true *)
(*         else if divides_evenly(n,c) *)
(*         then false *)
(*         else has_no_divisors(n,c-1) *)
(* in *)
(* fun is_prime (n) *)
(*   = has_no_divisors(n,n-1) *)
(* end *)

fun is_prime (n) =
  has_no_divisors(n,n-1)
and has_no_divisors (n,c) =
    if eq_int(c,1)
    then true
    else if divides_evenly(n,c)
    then false
    else has_no_divisors(n,c-1)

(* better version *)
fun is_prime (n) =
  let fun has_no_divisors (n,c) =
        if eq_int (c,1)
        then true
        else if eq_int(n,c)
        then has_no_divisors(n,c-1)
        else if divides_evenly(n,c)
        then false
        else has_no_divisors(n,c-1)
  in
    has_no_divisors(n,n)
  end

(* another long chain link *)
fun primes (n) =
  if is_prime(n)
  then Link(n,primes)
  else primes(n+1)

fun fibs (n)(m) =
  Link(n+m,fibs(m))

(* it is like fibs, without (n) *)
fun fibs_1 (m) =
  Link(1 + m,fibs(m))

fun fibs_2 (m) =
  Link(2 + m,fibs(m))
