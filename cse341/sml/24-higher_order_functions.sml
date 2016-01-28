fun n_times (f, n, x) =
  if n=0
  then x
  else f (n_times(f, n-1, x))

fun double x = x + x
fun increment x = x + 1
val x1 = n_times(double, 4, 7)
val x2 = n_times(increment, 4, 7)
val x3 = n_times(tl, 2, [4, 8, 12, 16])

fun double_n_times (n, x) = n_times(double, n, x)
fun nth_tail (n, x) = n_times(tl, n, x)

fun desc x = x - 1
fun times_until_0 (f, x) =
  if x=0 then 0 else 1 + times_until_0(f, f x)
