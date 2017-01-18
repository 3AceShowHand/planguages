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

fun f x = 
    case x of
         Pizza => 3
       | Str s => 8
       | TwoInts(i1, i2) => i1 + i2

f Pizza
f (Str "hi")

(*Useful Datatypes*)
datatype suit = Club | Diamond | Heart | Spade
datetype rank = Jack | Queen | King | Ace | Num of int

datatype id = StudentNum of int
            | Name of string * (string option) * string

(*self-reference*)
datatype exp = Constant of int
             | Negate   of exp
             | Add      of exp * exp
             | Multiply of exp * exp
Add(Constant (10+9), Negate(Constant 4))

fun eval e = 
    case e of
            Constant i => i
       |    Negate e2 => ~(eval e2)
       |    Add(e1, e2) => (eval e1) + (eval e2)
       |    Multiply(e1, e2) => (eval e1) * (eval e2)

fun number_of_adds e = 
    case e of 
            Constant i => 0
        |   Negate e2 => (number_of_adds e2)
        |   Add(e1, e2) => 1 + (number_of_adds e1) + (number_of_adds e2)
        |   Multiply(e1, e2) => (number_of_adds e1) + (number_of_adds e2)

fun max_constant e = 
    let fun max_of_two(e1, e2) = 
        Int.max(max_constant e1, max_constant e2)
    in
    case e of 
        Constant i => i
    |   Negate e2 => max_constant e2
    |   Add(e1, e2) => max_of_two(e1, e2)
    |   Multiply(e1, e2) => max_of_two(e1, e2)
    end



val test_exp = Add(Constant 19, Negate(Constant 4))
val nineteen = max_constant test_exp