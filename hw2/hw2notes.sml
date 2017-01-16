(*Building Compound Types*)
val x = { bar=(1+2, true andalso true) , foo=3+4, baz=(false, 9) }
val niece = { name="Ameilia", id=422123}
val name = #name niece

(*Tuples as Syntactic sugar*)
val y = { 3="hi", 1=true, 2=3+2}

(*Datatype bindings*)
(*Any value of type mytype is made from one of the constructors.*)
datatype mytype = TwoInts of int * int
                | Str of string
                | Pizza

val a = Str "hi"
val b = Str
val c = Pizza
val d = TwoInts(1+2, 3+4)
val e = a

(*How to use datatype
1. Check what variant it is(what constructor made it)
2. Extract the data (if that variant has any)
*)
(*Case expression*)


