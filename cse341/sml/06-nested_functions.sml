fun countup_from1(x : int) =
  let
      (* give 3, 6 and return [3, 4, 5, 6] *)
      fun count (from : int) =
        if from = x
        then x :: []
        else from :: count (from+1)
  in
      count 1
  end
