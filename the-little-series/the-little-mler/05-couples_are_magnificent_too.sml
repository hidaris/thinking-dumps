(* Write the first draft of a function flowing
   all the morals. When it is correct and no sooner, simplify. *)

datatype 'a pizza = Bottom
                  | Topping of ('a * ('a pizza))

datatype fish = Anchovy
              | Lox
              | Tuna

datatype num = Zero
             | One_more_than of num

fun rem_anchovy (Bottom)
  = Bottom
  | rem_anchovy (Topping(Anchovy,p))
    = rem_anchovy(p)
  | rem_anchovy (Topping(t,p))
    = Topping(t,rem_anchovy(p))

fun rem_tuna (Bottom)
  = Bottom
  | rem_tuna (Topping(Tuna,p))
    = rem_tuna(p)
  | rem_tuna (Topping(t,p))
    = Topping(t,rem_tuna(p))

fun rem_fish (x,Bottom)
  = Bottom
  | rem_fish (Tuna,Topping(Tuna,p))
    = rem_fish(Tuna,p)
  | rem_fish (Tuna,Topping(t,p))
    = Topping(t,rem_fish(Tuna,p))
  | rem_fish (Anchovy,Topping(Anchovy,p))
    = rem_fish(Anchovy,p)
  | rem_fish (Anchovy,Topping(t,p))
    = Topping(t,rem_fish(Anchovy,p))
  | rem_fish (Lox,Topping(Lox,p))
    = rem_fish(Lox,p)
  | rem_fish (Lox,Topping(t,p))
    = Topping(t,rem_fish(Lox,p))

(* more clumsy *)
fun rem_fish (x,Bottom)
  = Bottom
  | rem_fish (Tuna,Topping(Tuna,p))
    = rem_fish(Tuna,p)
  | rem_fish (Tuna,Topping(Anchovy,p))
    = Topping(Anchovy,rem_fish(Tuna,p))
  | rem_fish (Tuna,Topping(Lox,p))
    = Topping(Lox,rem_fish(Tuna,p))
  | rem_fish (Anchovy,Topping(Anchovy,p))
    = rem_fish(Anchovy,p)
  | rem_fish (Anchovy,Topping(Tuna,p))
    = Topping(Tuna,rem_fish(Anchovy,p))
  | rem_fish (Anchovy,Topping(Lox,p))
    = Topping(Lox,rem_fish(Anchovy,p))
  | rem_fish (Lox,Topping(Lox,p))
    = rem_fish(Lox,p)
  | rem_fish (Lox,Topping(Anchovy,p))
    = Topping(Anchovy,rem_fish(Lox,p))
  | rem_fish (Lox,Topping(Tuna,p))
    = Topping(Tuna,rem_fish(Lox,p))

fun eq_fish (Anchovy,Anchovy)
  = true
  | eq_fish (Anchovy,Lox)
    = false
  | eq_fish (Anchovy,Tuna)
    = false
  | eq_fish (Lox,Anchovy)
    = false
  | eq_fish (Lox,Lox)
    = true
  | eq_fish (Lox,Tuna)
    = false
  | eq_fish (Tuna,Anchovy)
    = false
  | eq_fish (Tuna,Lox)
    = false
  | eq_fish (Tuna,Tuna)
    = true

(* simplify *)
fun eq_fish (Anchovy,Anchovy)
  = true
  | eq_fish (Lox,Lox)
    = true
  | eq_fish (Tuna,Tuna)
    = true
  | eq_fish (a_fish,another_fish)
    = false

fun rem_fish (x,Bottom)
  = Bottom
  | rem_fish (x,Topping(t,p))
    = if eq_fish(t,x)
      then rem_fish(x,p)
      else Topping(t,(rem_fish(x,p)))

fun eq_int (n:int,m:int) = (n = m)

fun rem_int (x,Bottom)
  = Bottom
  | rem_int (x,Topping(t,p))
    = if eq_int(t,x)
      then rem_int(t,p)
      else Topping(t,(rem_int(x,p)))

fun subst_fish (n,a,Bottom)
  = Bottom
  | subst_fish (n,a,Topping(t,p))
    = if eq_fish(t,a)
      then Topping(n,subst_fish(n,a,p))
      else Topping(t,subst_fish(n,a,p))

fun subst_int (n,a,Bottom)
  = Bottom
  | subst_int (n,a,Topping(t,p))
    = if eq_int(t,a)
      then Topping(n,subst_int(n,a,p))
      else Topping(t,subst_int(n,a,p))

fun eq_num (Zero,Zero)
  = true
  | eq_num (
      One_more_than(n),
      One_more_than(m))
    = eq_num(n,m)
  | eq_num (n,m)
    = false
