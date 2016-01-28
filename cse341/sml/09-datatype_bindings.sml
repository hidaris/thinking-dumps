datatype mytype = TwoInt of int * int
                | Str of string
                | Pizza

val a = Str "hi"
val b = Str
val c = Pizza
val d = TwoInt(1+2, 3+4)
val e = a
