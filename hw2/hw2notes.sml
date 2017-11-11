(*In any language, there are 3 most important type.
- Each of : A t value contains values of each ot t1, t2 ... tn
- One of: A t value contains values of one of t1, t2 ... tn
- Self reference: A t value can refer to other t values
*)

(*Building Compound Types*)

(* Records: A way to build and use each-of types *)
val x = { bar=(1+2, true andalso true) , foo=3+4, baz=(false, 9) }
val niece = { name="Amelia", id=422123 }
val name = #name niece
(*Tuples as Syntactic sugar, just another easier way to represent Record type.*)
val y = { 3="hi", 1=true, 2=3+2 }


(* Datatype bindings: A way to build and use one-of types *)
(* Any value of type mytype is made from one of the constructors.*)
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
(* mytype -> int *)
(* Type-checking: all branches must have same type *)
fun f (x : mytype) = 
    case x of
         Pizza => 3
       | Str s => String.size s
       | TwoInts(i1, i2) => i1 + i2

f Pizza
f (Str "hi")

(*Useful Datatypes*)
(* Enumerations *)
datatype suit = Club | Diamond | Heart | Spade
datatype rank = Jack | Queen | King | Ace | Num of int

datatype id = StudentNum of int
              | Name of string * (string option) * string

(*self-reference*)
datatype exp = Constant of int
                | Negate   of exp
                | Add       of exp * exp
                | Multiply of exp * exp

val example_exp : exp = Add(Constant (10+9), Negate(Constant 4))

fun eval e =
    case e of
            Constant i        => i
          | Negate e2         => ~ (eval e2)
          | Add(e1, e2)       => (eval e1) + (eval e2)
          | Multiply(e1, e2) => (eval e1) * (eval e2)

val example_eval : int = eval example_exp

fun number_of_adds e = (* exp -> int *)
    case e of
            Constant i => 0
          | Negate e2  => (number_of_adds e2)
          | Add(e1, e2) => 1 + (number_of_adds e1) + (number_of_adds e2)
          | Multiply(e1, e2) => (number_of_adds e1) + (number_of_adds e2)

val example_addcount : int = number_of_adds (Multiply(example_exp, example_exp))

fun max_constant e = 
    let fun max_of_two(e1, e2) = 
        Int.max(max_constant e1, max_constant e2)
    in
    case e of 
      Constant i => i
    | Negate e2 => max_constant e2
    | Add(e1, e2) => max_of_two(e1, e2)
    | Multiply(e1, e2) => max_of_two(e1, e2)
    end

fun max_constant2 e = 
    case e of
      Constant i => i
    | Negative e2 => max_constant2 e2
    | Add (e1,e2) => Int.max(max_constant2 e1, max_constant2 e2)
    | Multiply (e1, e2) => Int.max(max_constant2 e1, max_constant2 e2)


val test_exp = Add(Constant 19, Negate(Constant 4))
val nineteen = max_constant test_exp

(*Type Synonyms*)
type card = suit * rank
type name_record = { StudentNum: int option,
                        first: string,
                        middle: string option,
                        last: string}

fun is_Queen_of_Spade(c: card) = 
    #1 c = Spade andalso #2 c = Queen

val c1 : card = (Diamond, Ace)
val c2 : suit * rank = (Heart, Ace)
val c3 = (Spade, Ace)

fun is_Queen_of_Spade2 c = 
    case c of
      (Spade, Queen) => true
    | _ => false

(*Lists and Options are datatypes*)
datatype mylist = Empty
                | Cons of int * mylist

val x = Cons(4, Cons(23, Cons(2009, Empty)))

fun append_list(xs, ys) = 
    case xs of
      Empty => ys
    | Cons(x, xs') => Cons(x, append_list(xs', ys))

(* NONE and SOME are constructors, which can be used to build values 
, or use in pattern matching. *)
fun inc_or_zero intoption = 
    case intoption of 
      NONE => 0
    | SOME i => i+1

fun sum_list xs = 
    case xs of
      [] => 0
    | x::xs' => x + sum_list xs'

fun append (xs, ys) =
    case xs of 
      [] => ys
    | x::xs' => x::append(xs', ys)

(* 
* Pattern-matching is better for options and lists for the 
same reasons as for all datatypes
*)

(*Polymorphic Datatypes*)
datatype 'a option = NONE | SOME of 'a
datatype 'a mylist = Empty | Cons of 'a * 'a mylist

datatype ('a, 'b) tree = Node of 'a * ('a, 'b) tree * ('a, 'b) tree
                       | Leaf of 'b

(*type is (int, int) tree -> int*)
fun sum_tree tr = 
    case tr of
        Leaf i => i
    |   Node(i, left, right) => i + sum_tree left + sum_tree right

(* type is ('a, int) tree -> int *)
fun sum_leaves tr = 
    case tr of
        Leaf i => i
    |   Node(i, left, right) => sum_leaves left + sum_leaves right

fun num_leaves tr = 
    case tr of
        Leaf i => 1
      | None(i, left, right) => num_leaves left + num_leaves right


(* flower type can hold no data on its internal nodes, but each leaf
can hold one of two different types of data.*)
datatype ('a, 'b) flower = 
    Node of ('a, 'b) flower * ('a, 'b) flower
    | Leaf of 'a
    | Petal of 'b

(*
* Every val-binding and function-binding uses pattern-matching
* Every function in ML takes exactly one argument
*)

(* Val-binding can use a pattern, not just a variable.
 Turns out variables are just one kind of pattern. *)


(* Bad styles*)
fun sum_triple1 triple = 
    case triple of 
        (x, y, z) => x + y + z

fun full_name r = 
    case r of 
        {first=x, middle=y, last=z} => x ^ " " ^ y ^ " " ^ z

fun sum_triple1 triple = 
    let val (x, y, z) = triple
    in
        x + y + z
    end

(* A function argument can also be a pattern *)
(* Every function in ML takes one argument, not many.*)
fun sum_triple2 (x, y, z) = 
    x + y + z

(* A litter type reference *)
(* Type-checker can use patterns to figure out the types *)

(* These functions are polymorphic: type of y can be anything *)
(* int * 'a * int -> int *)
fun partial_sum (x, y, z) = 
    x + z

(*Polymorphic and Equality Types*)

(* 'a list * 'a list -> 'a list *)
(* 'a can be any type *)
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
if p is variable, the match succeeds and x is bound to v.
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

fun zip3 lst_triple = 
    case lst_triple of
        ([], [], []) => []
      | (hd1::tl1, hd2::tl2, hd3:: tl3) => (hd1, hd2, hd3)::zip3(tl1, tl2, tl3)
      | _ => raise ListLengthMismatch

fun unzip3 lst = 
    case lst of
        [] => ([], [], [])
      | (a, b, c) :: tl => let 
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
      | _ :: [] => true
      | head :: (neck :: rest) => head <= neck 
                                      andalso nondecreasing (neck :: rest)

datatype sng = P | N | Z

fun multsign (x1, x2) = 
    let 
        fun sign x = 
            if x = 0 then Z 
                      else if x > 0 then P else N
    in
        case (sign x1, sign x2) of
            (Z, _) => Z
          | (_, Z) => Z
          | (P, P) => P
          | (N, N) => P
          (*_ matches all others cases.*)
          | _ => N
    end

fun len xs =
    case xs of
        [] => 0
      | _ :: xs' => 1 + len xs'

(* Nested Patterns Precisely
* If p is a variable x, the match succeeds and x is bound to v
* If p is _, the match succeeds and no bindings are introduced
* If p is (p1, ..., pn) and v is (v1, ..., vn), the match succeeds
if and only if p1 matches v1, ..., pn matches vn. The bindings are the
union of all bindings from the submatches
* If p is 'C p1', the match succeeds if v is 'C v1' and p1 matches v1. 
(The same constructor). The bindings are the bindings from the submatch. *)

(*Function patterns*)
fun eval (Constant i) = i
  | eval (Negate e2) = ~ (eval e2)
  | eval (Add (e1, e2)) = (eval e1) + (eval e2)
  | eval (Multiply(e1, e2)) = (eval e1) * (eval e2)

fun eval e =
    case e of
            Constant i        => i
          | Negate e2         => ~ (eval e2)
          | Add(e1, e2)       => (eval e1) + (eval e2)
          | Multiply(e1, e2)  => (eval e1) * (eval e2)

fun append([], ys) = ys
  | append(x::xs', ys) = x :: append(xs', ys)

(* In general
fun f x = 
    case x of
        p1 => e1
      | p2 => e2
      ...
Can be written as

fun f p1 = e1
  | f p2 = e2
  ...
  | f pn = en
*)

(*Exceptions*)
fun hd xs = 
    case xs of
        [] => raise List.Empty
    |   x::_ => x

exception MyUndesirableCondition

exception MyOtherException of int * int
raise MyOtherException(3, 4)

fun mydiv (x, y) = 
    if y = 0
    then raise MyUndesirableCondition
    else x div y

(* exn: type of all exceptions *)
(* 
* Declaring an exception makes adds a constructor for type exn
* Can pass values of exn anywhere
* Handle can have multiple branches with patterns for type exn
 *)
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
The result of recursive calls is the result for the caller.

- Create a helper function that takes an accumulator
- Old base case becomes initial accumulator.
- New base case becomes final accumulator.
*)
fun fact n =
    let fun aux(n, acc) = 
        if n = 0
        then acc
        else aux(n - 1, acc * n)

    in 
        aux(n, 1)
    end
    
val x = fact 3

fun sum xs = 
    let fun aux (xs, acc) = 
        case xs of
            [] => acc
        |   x :: xs' => aux(xs', acc + x)
    in
        aux(xs, 0)
    end

fun rev xs = 
    case xs of
        [] => []
    |   x :: xs' => (rev xs') @ [x] (*beware of list-append, especially within outer recursion.*)

fun rev2 xs = 
    let fun aux(xs, acc) = 
        case xs of
            [] => acc
        |   x :: xs' => aux(xs', x :: acc)
    in
        aux(xs, [])
    end

(* A tail call is a function call in tail position
- In fun f p = e, the body e is in tail position
- If e1 then e2 else e3 is in tail position, then e2 and e3 are in tail position.
- if let b1, ..., bn in e end is in tail position, then e is in tail position.
- Function-call argument e1 e2 are not in tail position.
 *)

fun doIt q = 
    case q of
        [] => A   (* Tail call *)
    |   x :: xs => if B
                   then C    (* Tail call *)
                   else let 
                            val res = D
                        in 
                            E :: xs  
                        end    
