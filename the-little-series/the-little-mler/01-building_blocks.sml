(* Use datatype to describe types. When a
   type contains lots of values, the datatype
   definition refers to itself. Use 'a with
   datatype to define shapes. *)

datatype seasoning =
         Salt
         | Pepper

datatype num =
         Zero
         | One_more_than of num

datatype 'a open_faced_sandwich =
         Bread of 'a
         | Slice of 'a open_faced_sandwich
