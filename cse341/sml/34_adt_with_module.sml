signature RATIONAL_A =
sig
    datatype rational = Whole of int | Frac of int*int
    exception BadFrac
    val make_frac : int * int -> rational
    val add : rational * rational -> rational
    val toString : rational -> string
end

signature RATIONAL_B =
sig
    type rational
    exception BadFrac
    val make_frac : int * int -> rational
    val add : rational * rational -> rational
    val toString : rational -> string
end

signature RATIONAL_C =
sig
    type rational
    exception BadFrac
    (* still does not allow using Whole as a pattern *)
    val Whole : int -> rational (* still hiding the rest of the datatype *)
    val make_frac : int * int -> rational
    val add : rational * rational -> rational
    val toString : rational -> string
end


structure Rational1 :> RATIONAL_B =
struct
datatype rational = Whole of int | Frac of int*int
exception BadFrac

(* internal functions gcd and reduce not on slide *)
fun make_frac (x, y) = ...
fun add (r1, r2) = ...
fun toString r = ...
end

structure Rational3 =
struct
type rational = int * int
exception BadFrac

(* internal functions gcd and reduce not on slide *)
fun make_frac (x, y) = ...
fun Whole i = (i, 1)            (* needed for RATIONAL_C *)
fun add ((a, b)(c, d)) = (a*d+b*c, b*d)
fun toString r = ...            (* reduce at last minute *)
end
