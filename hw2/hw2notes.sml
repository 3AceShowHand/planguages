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
datatype rank = Jack | Queen | King | Ace | Num of int

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

(*Type Synonyms*)
type card = suit * rank
type name_record = { StudentNum: int option,
                     first     : string,
                     middle    : string option,
                     last      : string}

fun is_Queen_of_Spade(c: card) = 
    #1 c = Spade andalso #2 c = Queen

val c1 : card = (Diamond, Ace)
val c2 : suit * rank = (Heart, Ace)
val c3 = (Spade, Ace)

fun is_Queen_of_Spade2 c = 
    case c of
        (Spade, Queen) => true
    | _=> false

(*Lists and Options are datatypes*)

datatype mylist = Empty
                | Cons of int * mylist

val x = Cons(4, Cons(23, Cons(2009, Empty)))

fun append_list(xs, ys) = 
    case xs of
        Empty => ys
    |   Cons(x, xs') => Cons(x, append_list(xs', ys))

fun inc_or_zero intoption = 
    case intoption of 
        NONE => 0
    |   SOME i => i+1

fun sum_list xs = 
    case xs of
        [] => 0
    | x::xs' => x + sum_list xs'

fun append (xs, ys) =
    case xs of 
        [] => ys
    |   x::xs' => x::append(xs', ys)


(*Polymorphic Datatypes*)
datatype 'a option = NONE | SOME of 'a
datatype 'a mylist = Empty | Cons OF 'a * 'a mylist

datatype ('a, 'b) tree = Node of 'a * ('a, 'b) tree * ('a, 'b) tree
                       | Leaf of 'b
(*type is (int, int) tree -> int*)
fun sum_tree tr = 
    case tr of
        Leaf i => i
    |   Node(i, left, right) => i + sum_tree left + sum_tree right

fun sum_leaves tr = 
    case tr of
        Leaf i => i
    |   Node(i, left, right) => sum_leaves left + sum_leaves right

(*Eevery val-binding and function-binding uses pattern-matching
  Every function in ML takes exactly one argument*)

(*A litter type reference*)
fun sum_triple (x, y, z) = 
    x + y + z

(* int*'a*int->int *)
fun partial_sum (x, y, z) = 
    x + z

(*Polymorphic and Equality Types*)

(* 'a list * 'a list -> 'a list *)
fun append(xs, ys) = 
    case xs of
        [] => ys
    |   x::xs' => x :: append(xs', ys)

(* ''a list * ''a list -> bool 
''a must be replaced by types could use =operator,
only same type could be compared.*)

(* ''a * ''a -> string *)
fun same_thing(x, y) = 
    if x = y then "yes" else "no"

(* int -> string*)
(* in this function, compiler know that x must be int.*)
fun is_three x = 
    if x = 3 then "yes" and "no"

(*Nested Patterns*)
(*pattern-matching takes pattern p and value v
if p is variablex, the match succeeds and x is bound to v.
if p is _, the match succeeds and no bindings are introduced.
if p is (p1, ..., pn) and v is (v1, ..., vn), the match succeeds if and only if
pn matches vn. 
The bindings are the union of all bindings from the submatches.
if p is constructor p1, the match succeeds if v1 is a value build with the same constructor
*)

(*
a :: b :: c :: d matches all lists with >= 3 elements.
a :: b :: c :: [] matches all lists with 3 elements.
((a,b), (c,d)) :: e matches all non-empty lists of pairs of pairs.
*)

(* Avoid nested case expressions if nested patterns are simpler
and avoid unnecessary branches or let-expressions*)
exception ListLengthMismatch

fun zip3 list_triple = 
    case list_triple of
        ([], [], []) => []
    |   (hd1::tl1, hd2::tl2, hd3:: tl3) => (hd1, hd2, hd3)::zip3(tl1, tl2, tl3)
    |   _ => raise ListLengthMismatch

fun unzip3 lst = 
    case lst of
        [] => ([], [], [])
    |   (a, b, c) :: tl => let 
                                val (l1, l2, l3) = unzip3 tl
                            in
                                (a::l1, b::l2, c::l3)
                            end

zip3 ([1, 2, 3], [4, 5, 6], [7, 8, 9])
unzip3[(1, 4, 7), (2, 5, 8), (3, 6, 9)]

(*More Nested Patterns*)
(* Wildcard are good style.*)
fun nondecreasing xs =
    case xs of
        [] => true
    |   _ :: [] => true
    |   head :: (neck :: rest) => head <= neck andalso nondecreasing (neck :: rest)

datatype sng = P | N | Z

fun multsign (x1, x2) = 
    let 
        fun sign x = 
            if x = 0 then Z else if x > 0 then P else N
    in
        case (sign x1, sign x2) of
            (Z, _) => Z
        |   (_, Z) => Z
        |   (P, P) => P
        |   (N, N) => P
        (*_ matches all others cases.*)
        |   _ => N
    end

fun len xs =
    case xs of
        [] => 0
    |   _ :: xs' => 1 + len xs'

(*Function patterns*)
fun eval (Constant i) = i
  | eval (Negate e2) = ~ (eval e2)
  | eval (Add (e1, e2)) = (eval e1) + (eval e2)
  | eval (Multiply(e1, e2)) = (eval e1) * (eval e2)

fun f p1 = e1
  | f p2 = e2
  ...
  | f pn = en

(*Exceptions*)

fun hd xs = 
    case xs of
        [] => raise List.Empty
    |   x::_ => x

exception MyUndesirableCondition

exception MyOtherException of int * int

fun mydiv (x, y) = 
    if y = 0
    then raise MyUndesirableCondition
    else x div y

fun maxlist(xs, ex) =  (* int list * exn -> int *)
    case xs of
        [] => raise ex
    |   x :: [] => x
    |   x :: xs' => Int.max(x, maxlist(xs', ex))

val w = maxlist ([3, 4, 5], MyUndesirableCondition)

val x = maxlist ([3, 4, 5], MyUndesirableCondition)
        handle MyUndesirableCondition => 42

val z = maxlist ([], MyUndesirableCondition)
        handle MyUndesirableCondition => 42

(*Tail Recursion
-Using an accumulator to achieve tail recursion
*)
