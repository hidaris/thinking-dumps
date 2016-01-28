datatype mytype = TwoInt of int * int
                | Str of string
                | Pizza

(* mytype -> int *)
fun f (x : mytype) =
  case x of
      Pizza => 3
    | Str s  => String.size s
    | TwoInt(i1, i2) => i1 + i2
