(* Rational1和Rational2的区别在于在允许直接以Frac构造rational的时候，
   Rational2在tostring时进行reduce，Rational1不进行，在B和C的情况下，屏蔽了
   Frac，所以都会进行reduce *)

signature RATIONAL_A =
sig
    datatype rational = Whole of int | Frac of int*int
    exception BadFrac
    val make_frac : int * int -> rational
    val add : rational * rational -> rational
    val toString : rational -> string
end

structure Rational2 :> RATIONAL_A (* or B or C *) =
struct

(* Invariant 1: all denominators > 0
   Invariant 2: rationals kept in reduced form *)

datatype rational = Whole of int | Frac of int*int
exception BadFrac

(* gcd and reduce help keep fractions reduced,
   but clients need not know about then *)
(* they _assume_ their inputs are not negative *)

(* when making a frac, we ban zero denominators *)
fun make_frac (x,y) =
  if y=0
  then raise BadFrac
  else if y < 0
  then Frac(~x,~y)
  else Frac(x,y)

(* using math properties, both invariants hold of the result
   assuming they hold of the arguments *)
fun add (r1, r2) =
  case (r1, r2) of
      (Whole(i),Whole(j))   => Whole(i+j)
    | (Whole(i),Frac(j,k))  => Frac(j+k*i, k)
    | (Frac(j,k),Whole(i))  => Frac(j+k*i,k)
    | (Frac(a,b),Frac(c,d)) => Frac(a*d + b*c, b*d)

(* given invariant, prints in reduced form *)
fun toString r =
  let fun gcd (x, y) =
        if x=y
        then x
        else if x < y
        then gcd(x, y-x)
        else gcd(y, x)

      fun reduce r =
        case r of
            Whole _ => r
          | Frac(x,y) =>
            if x=0
            then Whole 0
            else let val d = gcd(abs x, y) in (* using invariant 1 *)
                     if d=y
                     then Whole(x div d)
                     else Frac(x div d, y div d)
                 end
  in
      case reduce r of
          Whole i => Int.toString i
        | Frac(a,b) => (Int.toString a) ^ "/" ^ (Int.toString b)
  end
end
