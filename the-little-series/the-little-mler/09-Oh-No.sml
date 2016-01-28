(* Some functions produce exceptions instead of values;
   some don't produce any-thing. Handle raised exceptions carefully. *)

datatype 'a list = Empty
                 | Cons of 'a * 'a list

datatype box = Bacon
             | Ix of int   (* better name are bacon_or_index and Index *)

fun is_bacon (Bacon)
  = true
  | is_bacon (Ix(n))
    = false

exception No_bacon of int

fun where_is (Empty)
  = raise No_bacon(0)
  | where_is (Cons(a_box,rest))
    = if is_bacon(a_box)
      then 1
      else 1 + where_is(rest)

fun eq_int (n1:int, n2:int)
  = (n1 = n2)

exception Out_of_range

fun list_item (n,Empty)
  = raise Out_of_range
  | list_item (n,Cons(abox,rest))
    = if eq_int(n,1)
      then abox
      else list_item(n-1,rest)

(* It is used to restart the search for Bacon with a new index *)
fun find (n,boxes)
  = check(n,boxes,list_item(n,boxes))
    handle
    Out_of_range
    => find(n div 2,boxes)
and check (n,boxes,Bacon)
    = n
  | check (n,boxes,Ix(i))
    = find(i,boxes)

(* we just need n which raise exception *)
fun path (n,boxes)
  = Cons(n,
         (check(boxes,list_item(n,boxes))
          handle
          Out_of_range
          => path(n div 2,boxes)))
and check (boxes,Bacon)
    = Empty
  | check (boxes,Ix(i))
    = path(i,boxes)
