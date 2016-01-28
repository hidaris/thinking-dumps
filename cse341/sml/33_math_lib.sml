(* signature is a type for a module *)
signature MATHLIB =  (* signature SIGNAME = sig types-for-bindings end *)
sig
    val fact : int -> int
    val half_pi : real
    val doubler : int -> int
end

structure MyMathLib :> MATHLIB =
(* structure StructureName = struct bindings end*)
struct
fun fact x =
  if x=0
  then 1
  else x * fact(x-1)

val half_pi = Math.pi / 2
fun doubler x = x * 2
end
