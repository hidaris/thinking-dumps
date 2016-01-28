(*
   length : T1 -> T2
   xs : T1

   x : T3
   xs' : T3 list
   T1 = T3 list
   T2 = int
   T3 list -> int
   'a list -> int
*)
fun length xs =
  case xs of
      [] => 0
    | x::xs' => 1+(length xs')

(*
   f : T1 * T2 * T3 -> T4
   x : T1
   y : T2
   y : T3

   T4 = T1 * T2 * T3
   T4 = T2 * T1 * T3
   only way those can both be true is if T1 = T2
   put it all together: f : T1 * T1 * T3 -> T1 * T1 * T3
   'a * 'a * 'b -> 'a * 'a * 'b
*)
fun f (x,y,z) =
  if true
  then (x,y,z)
  else (y,x,z)

(*
   compose : T1 * T2 -> T3
   f : T1
   g : T2
   x : T4
   body being a function has type T3=T4->T5
   from g being passed x, T2=T4->T6 for some T6
   from f being passed the result of g, T1=T6->T7 for some T7
   from call to f being body of anoymous function T7=T5

   put it all together: T1=T6->T5, T2=T4->T6, T3=T4->T5
   (T6->T5) * (T4->T6) -> (T4->T5)
   ('a->'b) * ('c->'a) -> ('c->'b)
*)
fun compose (f,g) = fn x => f (g x)
