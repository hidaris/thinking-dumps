(* Some functions consume values of star
   types; some produce values of star type. *)

datatype meza = Shrimp
              | Calamari
              | Escargots
              | Hummus

datatype main = Steak
              | Ravioli
              | Chicken
              | Eggplant

datatype salad = Green
               | Cucumber
               | Greek

datatype desserts = Sundae
                  | Mousse
                  | Torte

fun add_a_steak (Shrimp) =
  (Shrimp,Steak)
  | add_a_steak (Calamari) =
    (Calamari,Steak)
  | add_a_steak (Escargots) =
    (Escargots,Steak)
  | add_a_steak (Hummus) =
    (Hummus,Steak)

fun add_a_steak (x : meza) : (meza * main)
  = (x,Steak)

fun eq_main(Steak,Steak) =
  true
  | eq_main (Ravioli,Ravioli) =
    true
  | eq_main (Chicken,Chicken) =
    true
  | eq_main (a_main,another_main) =
    false

fun has_steak (a:meza,Steak,d:desserts):bool =
  true
  | has_steak (a:meza,ns,d:desserts):bool =
    false
