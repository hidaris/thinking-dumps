(* The number and order of the patterns in the definition of a function
   should match that of the definition of the consumed datatype *)

datatype shish_kebab = Skewer
                     | Onion of shish_kebab
                     | Lamb of shish_kebab
                     | Tomato of shish_kebab

fun only_onions (Skewer)
  = true
  | only_onions (Onion(x))
    = only_onions(x)
  | only_onions (Lamb(x))
    = false
  | only_onions (Tomato(x))
    = false

fun is_vegetarian (Skewer)
  = true
  | is_vegetarian (Onion(x))
    = is_vegetarian(x)
  | is_vegetarian (Lamb(x))
    = false
  | is_vegetarian (Tomato(x))
    = is_vegetarian(x)

datatype 'a shish = Bottom of 'a
                  | Onion of 'a shish
                  | Lamb of 'a shish
                  | Tomato of 'a shish

datatype rod = Dagger
             | Fork
             | Sword

datatype plate = Gold_plate
               | Siver_plate
               | Brass_plate

fun is_veggie (Bottom(x))
  = true
  | is_veggie (Onion(x))
    = is_veggie (x)
  | is_veggie (Lamb(x))
    = false
  | is_veggie (Tomato(x))
    = is_veggie (x)

fun what_bottom (Bottom(x))
  = x
  | what_bottom (Onion(x))
    = what_bottom (x)
  | what_bottom (Tomato(x))
    = what_bottom (x)
  | what_bottom (Lamb(x))
    = what_bottom (x)
