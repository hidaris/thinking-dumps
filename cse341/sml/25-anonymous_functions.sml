fun map (f, xs) =
  case xs of
      [] => []
    | x::xs' => (f x)::(map (f, xs'))

fun filter (f, xs) =
  case xs of
      [] => []
    | x::xs' => if f x
                then x::(filter(f, xs'))
                else filter(f, xs')

fun double_or_trible f =
  if f 7
  then fn x => 2*x
  else fn x => 3*x
