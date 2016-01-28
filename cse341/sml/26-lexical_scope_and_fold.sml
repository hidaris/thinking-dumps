fun greaterThanX x = fn y => y > x

fun filter (f, xs) =
  case xs of
      [] => []
    | x::xs' => if f x
                then x::filter(f, xs')
                else filter(f, xs')

fun noNegatives xs = filter(greaterThanX ~1, xs)
fun allGreater (xs, n) = filter(fn x => x > n, xs)

fun allShorterThan1 (xs, s) =
  filter(fn x => String.size x < String.size s, xs)

fun allShorterThan2 (xs, s) =
  let val i = String.size s
  in filter(fn x => String.size x < i, xs) end

fun fold (f, acc, xs) =
  case xs of
      [] => acc
    | x::xs' => fold(f, f(acc, x), xs')

fun f1 xs = fold((fn (x, y) => x+y), 0, xs)
fun f2 xs = fold((fn (x, y) => x andalso y>0),
                 true, xs)

fun f3 (xs, hi, lo) =
  fold((fn (x, y) =>
           x + (if y >= lo andalso y <= hi
                then 1
                else 0)),
       0, xs)

fun f4 (g, xs) =
  fold(fn (x, y) => x andalso g y, true, xs)
