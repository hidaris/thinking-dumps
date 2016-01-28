(* Functions that produce values of a
   datatype must use the associated constructors to build data of that type *)
datatype pizza = Crust
               | Cheese of pizza
               | Onion of pizza
               | Anchovy of pizza
               | Sausage of pizza

fun remove_anchovy (Crust)
  = Crust
  | remove_anchovy (Cheese(x))
    = Cheese(remove_anchovy(x))
  | remove_anchovy (Onion(x))
    = Onion(remove_anchovy(x))
  | remove_anchovy (Anchovy(x))
    = remove_anchovy(x)
  | remove_anchovy (Sausage(x))
    = Sausage(remove_anchovy(x))

fun top_anchovy_with_cheese (Crust)
  = Crust
  | top_anchovy_with_cheese (Cheese(x))
    = Cheese(top_anchovy_with_cheese(x))
  | top_anchovy_with_cheese (Onion(x))
    = Onion(top_anchovy_with_cheese(x))
  | top_anchovy_with_cheese (Anchovy(x))
    = Cheese(
      Anchovy(
        top_anchovy_with_cheese(x)))
  | top_anchovy_with_cheese (Sausage)
    = Sausage(top_anchovy_with_cheese(x))

fun subst_anchovy_by_cheese (x)
  = remove_anchovy(
    top_anchovy_with_cheese(x))

fun subst_anchovy_by_cheese2 (Crust)
  = Crust
  | subst_anchovy_by_cheese2 (Cheese(x))
    = Cheese(subst_anchovy_by_cheese2(x))
  | subst_anchovy_by_cheese2 (Onion(x))
    = Onion(subst_anchovy_by_cheese2(x))
  | subst_anchovy_by_cheese2 (Anchovy(x))
    = Cheese(subst_anchovy_by_cheese2(x))
  | subst_anchovy_by_cheese2 (Sausage(x))
    = Sausage(subst_anchovy_by_cheese2(x))
