(* type is int list -> int *)
fun sum_list xs =
  case xs of
      [] =>
    | x::xs' => x + sum_list xs'

(* type is 'a list * 'a list -> 'a list *)
fun append (xs, ys) =
  case xs of
      [] => ys
    | x::xs' => x::append(xs', ys)

(* this really is /exactly/ how options are defined
   careful: now shadowing the built in ones!
 *)

datatype 'a option = NONE | SOME of 'a

(* similarly, here are polymorphic lists but without special syntax *)
datatype 'a mylist = Empty | Cons of 'a * 'a mylist

(* a fancier example for binary trees where internal nodes have data
any 1 type and leaves have data of another possibly-different type *)
datatype ('a, 'b) tree = Node of 'a * ('a, 'b) tree * ('a, 'b) tree
                       | Leaf of 'b

(* type is (int, int) tree -> int *)
fun sum_tree tr =
  case tr of
      Leaf i => i
    | Node(i, lft, rft) => i + sum_tree lft + sum_tree rft

(* type is ('a, int) tree -> int *)
fun sum_leaves tr =
  case tr of
      Leaf i => i
    | Node(i, lft, rft) => sum_leaves lft + sum_leaves rft

(* type is ('a, 'b) tree -> int *)
fun num_leaves tr =
  case tr of
      Leaf i => 1
    | Node(i, lft, rgt) => num_leaves lft + num_leaves rft
