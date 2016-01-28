type tvar = int

(* Cough, cough, ignore this bit. It lets us generate fresh type
 * variables but uses some features of SML we haven't talked about yet *)
local val freshSource = ref 0 in
fun fresh () : tvar =
    !freshSource before freshSource := !freshSource + 1
end

(* A normal (not polymorphic) type *)
datatype monotype = TBool
                  | TArr of monotype * monotype
                  | TVar of tvar

(* A polymorphic type which has binds a list of type variables *)
datatype polytype = PolyType of int list * monotype

(* Our definition of expressions in our language *)
datatype exp = True
             | False
             | Var of int
             | App of exp * exp
             | Let of exp * exp
             | Fn of exp
             | If of exp * exp * exp

(* A piece of information we know about a particular variable. We
   either know it has a polytype or a monotype *)
datatype info = PolyTypeVar of polytype
              | MonoTypeVar of monotype

(* A list of information. The ith entry is about the DeBruijn variable i *)
type context = info list

(* Substitute some type var for another type *)
fun subst ty' var ty =
    case ty of
        TVar var' => if var = var' then ty' else TVar var'
      | TArr (l, r) => TArr (subst ty' var l, subst ty' var r)
      | TBool => TBool

(* Collect a list of all the variables in a type *)
fun freeVars t =
    case t of
        TVar v => [v]
      | TArr (l, r) => freeVars l @ freeVars r
      | TBool => []
