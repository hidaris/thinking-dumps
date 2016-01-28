fun sum xs =
  case xs of
      [] => 0
    | x::xs' => x + sum xs'

fun sum2 xs =
  let fun aux(xs, acc) =
        case xs of
            [] => acc
          | x::xs' => aux(xs', acc+x)
  in
      aux(xs, 0)
  end

fun fact n =
  if n = 0
  then 1
  else n * fact(n-1)

fun fact2 n =
  let fun aux(n, acc) =
        if n=0
        then acc
        else aux(n-1, acc*n)
  in
      aux(n, 1)
  end

fun rev xs =
  case xs of
      [] => []
    | x::xs' => (rev xs') @ [x]

fun rev2 xs =
  let fun aux(xs, acc) =
        case xs of
            [] => acc
          | x::xs' => rev2(xs', x::acc)
  in
      aux(xs, [])
  end
