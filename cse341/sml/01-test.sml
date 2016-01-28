(* This is a comment. This is our first program. *)

val x = 34;
(* static environment: x : int *)
(* dynamic environment: x --> 34 *)

val y = 17;
(* static environment: y : int *)
(* dynamic environment: x -- 34, y --> 17 *)

val z = (x + y) + (y + 2);
(* static environment: z : int *)
(* dynamic environment: x --> 34, y --> 17, z --> 70*)

val q = z + 1;
(* static environment: x : int , y: int, z : int, q : int *)
(* dynamic environment: x --> 34, y --> 17, z --> 70, q --> 71 *)

val abs_of_z = if z < 0 then 0 - z else z;
(* abs_of_z : int *)
(* dynamic environment: ..., abs_of_z --> 70 *)

val abs_of_z_simpler = abs z;

fun sum_list (xs : int list) =
  if null xs
  then 0
  else hd(xs) + sum_list(tl(xs))
                        (*  *)
fun rev2 xs =
  let fun aux(xs, acc) =
        case xs of
            [] => acc
          | x::xs' => aux(xs', x::acc)
  in
      aux(xs, [])
  end;
