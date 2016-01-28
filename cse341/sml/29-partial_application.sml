fun fold f acc xs =
  case xs of
      [] => acc
    | x::xs' => fold f (f(acc, x)) xs'

fun sum_inferior xs = fold (fn (x, y) => x+y) 0 xs

fun range i j = if i > j then [] else i :: range (i+1) j

val countup = range 1

val sum = fold (fn (x, y) => x+y) 0

fun exists predicate xs =
  case xs of
      [] => false
    | x::xs' => predicate x
                orelse exists predicate xs'

val no = exists (fn x => x=7) [4, 11, 23]
val hasZero = exists (fn x => x=0)

val incrementAll = List.map (fn x => x+1)

val removeZeros = List.filter (fn x => x <> 0)

fun pairWithOne xs = List.map (fn x => (x,1)) xs

val pairWithOne : string list -> (string * int) list = List.map (fn x => (x, 1))

val pairWithOne = List.map (fn x => (x, 1))

val incrementAndPairWithOne = List.map (fn x => (x+1, 1))
