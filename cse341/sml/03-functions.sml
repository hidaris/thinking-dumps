fun pow (x : int, y : int) =
  if y = 0
  then 1
  else x * pow (x, y-1);
pow (2, 3);

fun cube (x : int) =
  pow (x, 3);

val sixtyfour = cube(4);
val fortytwo = pow (2, 4) + pow(4, 2) + cube(2) + 2;
