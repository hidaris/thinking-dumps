infix |>
fun x |> f = f x
fun sqrt_of_abs1 i = Math.sqrt(Real.fromInt(abs i))
fun sqrt_of_abs2 i = (Math.sqrt o Real.fromInt o abs) i
val sqrt_of_abs3 = Math.sqrt o Real.fromInt o abs
fun sqrt_of_abs i =
  i |> abs |> Real.fromInt |> Math.sqrt

fun backup1 (f,g) =
  fn x => case f x of
              NONE => g x
            | SOME y => y
