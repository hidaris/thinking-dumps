(* As datatype definitions get more
   complicated,so do the functions over them. *)

(* After we have designed a program that
   naturally follows the datatype definitions,
   we can considerably improve it by focusing
   on its weaknesses and carefully rearranging its pieces.*)

datatype fruit = Peach
               | Apple
               | Pear
               | Lemon
               | Fig

datatype tree = Bud
              | Flat of fruit * tree
              | Split of tree * tree

fun flat_only (Bud)
  = true
  | flat_only (Flat(f,t))
    = flat_only(t)
  | flat_only (Split(s,t))
    = false

fun split_only (Bud)
  = true
  | split_only (Flat(f,t))
    = false
  | split_only (Split(s,t))
    = if split_only(s)
      then split_only(t)
      else false

fun contains_fruit (Bud)
  = false
  | contains_fruit (Flat(f,t))
    = true
  | contains_fruit (Split(s,t))
    = if contains_fruit(s)
      then true
      else contains_fruit(t)

fun contains_fruit (x)
  = if split_only(x)
    then false
    else true

fun less_than (n:int,m:int) = (n < m)

fun larger_of (n,m)
  = if less_than (n,m)
    then m
    else n

fun height (Bud)
  = 0
  | height (Flat(f,t))
    = 1 + height(t)
  | height (Split(s,t))
    = 1 + larger_of(height(s),height(t))

fun eq_fruit (Peach,Peach)
  = true
  | eq_fruit (Apple,Apple)
    = true
  | eq_fruit (Pear,Pear)
    = true
  | eq_fruit (Lemon,Lemon)
    = true
  | eq_fruit (Fig,Fig)
    = true
  | eq_fruit (a_fruit,another_fruit)
    = false

fun subst_in_tree (n,a,Bud)
  = Bud
  | subst_in_tree (n,a,Flat(f,t))
    = if eq_fruit(f,a)
      then Flat(n,subst_in_tree(n,a,t))
      else Flat(f,subst_in_tree(n,a,t))
  | subst_in_tree (n,a,Split(s,t))
    = Split(
      subst_in_tree(n,a,s),
      subst_in_tree(n,a,t))

fun occurs (a,Bud)
  = 0
  | occurs (a,Flat(f,t))
    = if eq_fruit(f,a)
      then 1 + occurs(a,t)
      else occurs(a,t)
  | occurs (a,Split(s,t))
    = occurs(a,s) + occurs(a,t)

(* the first definition refers to the second
   and the second refers to the first *)
datatype 'a slist = Empty
                  | Scons of (('a sexp) * ('a slist))
     and 'a sexp = An_atom of 'a
                 | A_slist of ('a slist)

fun occurs_in_slist (a,Empty)
  = 0
  | occurs_in_slist (a,Scons(s,y))
    = occurs_in_sexp(a,s) +
      occurs_in_slist(a,y)
and occurs_in_sexp (a,An_atom(b))
    = if eq_fruit(b,a)
      then 1
      else 0
  | occurs_in_sexp (a,A_slist(y))
    = occurs_in_slist(a,y)

fun subst_in_slist (n,a,Empty)
  = Empty
  | subst_in_slist (n,a,Scons(s,y))
    = Scons(
      subst_in_sexp(n,a,s),
      subst_in_slist(n,a,y))
and subst_in_sexp (n,a,An_atom(b))
    = if eq_fruit(b,a)
      then An_atom(n)
      else An_atom(b)
  | subst_in_sexp (n,a,A_slist(y))
    = A_slist(subst_in_slist(n,a,y))

fun rem_from_slist (a,Empty)
  = Empty
  | rem_from_slist (a,Scons(s,y))
    = if eq_fruit_in_atom(a,s)
      then rem_from_slist(a,y)
      else Scons(
          rem_from_sexp(a,s),
          rem_from_slist(a,y))
and rem_from_sexp (a,An_atom(b))
    = An_atom(b)
  | rem_from_slist (a,A_slist(y))
    = A_slist(rem_from_slist(a,y))

fun eq_fruit_in_atom (a,An_atom(s))
  = eq_fruit(a,s)
  | eq_fruit_in_atom (a_fruit,A_slist(y))
    = false

fun rem_from_slist (a,Empty)
  = Empty
  | rem_from_slist (a,Scons(An_atom(b),y))
    = if eq_fruit(a,b)
      then rem_from_slist(a,y)
      else Scons(
          An_atom(b),
          rem_from_slist(a,y))
  | rem_from_slist (a,Scons(A_slist(x),A_slist(y)))
    = Scons(
      rem_from_slist(a,x),
      rem_from_slist(a,y))
