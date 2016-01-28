val sorted3 = fn x => fn y => fn z =>
                 z >=y andalso y >= x
val t1 = ((sorted3 7) 9) 11

fun sorted4 x y z = z >= y andalso y >= x
val t1 = sorted4 7 9 11


(* curried fold *)
fun fold f acc xs =
  case xs of
      []     => acc
    | x::xs' => fold f f(acc, x) xs'

fun sum xs = fold (fn (x, y) => x+y) 0 xs
