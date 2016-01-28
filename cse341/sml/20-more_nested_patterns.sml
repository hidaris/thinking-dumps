(* section2: More nested patterns *)

fun nondecreasing xs =          (* int list -> bool *)
  case xs of
      [] => true
    | _::[] => true
    | head::(neck::rest) => head <= neck
                            andalso nondecreasing (neck::rest)
datatype sgn = P | N | Z

fun multsign (x1, x2) =         (* int*int -> sgn *)
  let fun sign x = if x=0 then Z else if x>0 then P else N
  in

  end
