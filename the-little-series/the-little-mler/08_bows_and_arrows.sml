(* replace stars by arrows to reduce the number of values consumed and
   to increase the generality of the function defined. *)

datatype 'a list = Empty
                 | Cons of 'a * 'a list

datatype orapl = Orange
               | Apple

fun eq_orapl (Orange,Orange)
  = true
  | eq_orapl (Apple,Apple)
    = true
  | eq_orapl (a_orapl,another_orapl)
    = false

fun eq_int (n1:int,n2:int) = (n1 = n2)

fun subst_int (n,a,Empty)
  = Empty
  | subst_int (n,a,Cons(e,t))
    = if eq_int(a,e)
      then Cons(n,subst_int(n,a,t))
      else Cons(e,subst_int(n,a,t))

fun subst_orapl (n,a,Empty)
  = Empty
  | subst_orapl (n,a,Cons(e,t))
    = if eq_orapl(a,e)
      then Cons(n,subst_orapl(n,a,t))
      else Cons(e,subst_orapl(n,a,t))

(* rel = relation *)
fun subst (rel,n,a,Empty)
  = Empty
  | subst (rel,n,a,Cons(e,t))
    = if rel(a,e)
      then Cons(n,subst(rel,n,a,t))
      else Cons(e,subst(rel,n,a,t))

fun less_than (a1:int,a2:int)
  = (a1 < a2)

fun in_range ((small,large),x)
  = if less_than(small,x)
    then less_than(x,large)
    else false

(* a better name for subst_pred is substitute_using_a_predicate *)
fun subst_pred (pred,n,Empty)
  = Empty
  | subst_pred (pred,n,Cons(e,t))
    = if pred(e)
      then Cons(n,subst_pred(pred,n,t))
      else Cons(e,subst_pred(pred,n,t))

fun is_15 (n)
  = eq_int(n,15)

fun less_than_15 (x)
  = less_than(x,15)

fun in_range_11_16 (x)
  = in_range((11,16),x)

(* a better name for this function would be in_range_curry *)
fun in_range_c (small,large)(x)
  = if less_than(small,x)
    then less_than(x,large)
    else false

fun subst_c (pred)(n,Empty)
  = Empty
  | subst_c (pred)(n,Cons(e,t))
    = if pred(e)
      then Cons(n,subst_c(pred)(n,t))
      else Cons(e,subst_c(pred)(n,t))

fun subst_c2 (pred)
  = fn (n,Empty)
       => Empty
  | (n,Cons(e,t))
    => if pred(e)
       then Cons(n,subst_c2(pred)(n,t))
       else Cons(e,subst_c2(pred)(n,t))

fun subst_c_in_range_11_16 (n,Empty)
  = Empty
  | subst_c_in_range_11_16 (n,Cons(e,t))
    = if in_range_11_16(e)
      then
        Cons(n,
             subst_c_in_range_11_16(n,t))
      else
        Cons(e,
             subst_c_in_range_11_16(n,t))

fun combine (Empty,Empty)
  = Empty
  | combine (Empty,Cons(b,l2))
    = Cons(b,l2)
  | combine (Cons(a,l1),Empty)
    = Cons(a,l1)
  | combine (Cons(a,l1),Cons(b,l2))
    = Cons(a,combine(l1,Cons(b,l2)))

fun combine (Empty,l2)
  = l2
  | combine (Cons(a,l1),l2)
    = Cons(a,combine(l1,l2))

fun combine_c (Empty)(l2)
  = l2
  | combine_c (Cons(a,l1))(l2)
    = Cons(a,combine_c(l1)(l2))
(* we can define a function that is like the value of
   combine_c(
     Cons(1,
      Cons(2,
       Cons(3,
        Empty)))).
 *)
fun prefixer_123 (l2)
  = Cons(1,
         Cons(2,
              Cons(3,
                   l2)))
(* prefixer_123 is used on a list, but combine_c only seen first
   Cons when it is used. *)
fun waiting_prefix_123 (l2)
  = Cons(1,
         combine_c(
           Cons(2,
                Cons(3,
                     Empty)))
                  (l2))

fun base (l2)
  = l2
(* better name is combine_staged *)
fun combine_s (Empty)
  = base
  | combine_s (Cons(a,l1))
    = make_cons(a,combine_s(l1))
and make_cons (a,f)(l2)
    = Cons(a,f(l2))

(* make_cons(3,base)(l2) *)
fun prefix_3 (l2)
  = Cons(3,base(l2))

(* make_cons(2,prefix_3)(l2) *)
fun prefix_23 (l2)
  = Cons(2,prefix_3(l2))

(* make_cons(1,prefix_23)(l2) *)
fun prefix_123 (l2)
  = Cons(1,prefix_23(l2))
