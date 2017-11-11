(* Lexical scope *)

(* A function body is evaluated in the environment where 
the function was defined *)

val x = 1          (* x map to 1*)
fun f y = x + y    (* y = 1 + y *)

val x = 2         (* x map to 2*) 
val y = 3         (* y map to 3*)
val z = f ( x + y)  (* (z = 1 + 5) => 6 *)

(* Lexical scope: use environment where function is defined *)
(* Dynamic scope: use environment where function is called  *)

(* Fold and More Closures *)
fun fold (f, acc, xs) =
    case xs of
        [] => acc
       |x::xs => fold(f, f(acc, x), xs)

(* examples not using private data *)
fun f1 xs =
    fold ((fn (x, y) => x + y), 0, xs)

fun f2 xs =
    fold ((fn (x, y) => x andalso y >=0), true, xs)

(* examples using private data *)
fun f3 (xs, lo, hi) =
    fold ((fn (x, y) =>
                x + (if y >= lo andalso y <=hi then 1 else 0)),
                0, xs)

(* Combining functions *)
fun compose(f, g)=
    fn x => f(g x)

fun sqrt_of_abs i = Math.sqrt (Real.fromInt (abs i))

fun sqrt_of_abs2 i = (Math.sqrt o Real.fromInt o abs) i

val sqrt_of_abs = Math.sqrt o Real.fromInt o abs
(* val sqrt_of_abs3 i = i !> abs !> Real.fromInt !> Math.sqrt  *)

fun backup1 (f, g) =
    fn x => case f x of
                    NONE => g x
                   |SOME y => y

fun backup2 (f, g) =
    fn x => f x handle _ => g x

(* Curring 
- Recall every ML function takas exactly one argument
- previously encoded n arguments via one n-tuple

- Another way: Take one argument and return a function that
    takes another argument and ...
        --- Called "curring" after famous logician Haskell Curry
*)
fun sorted3_tupled (x, y, z) = z >= y andalso y >= z

val sorted3 =
    fn x => 
        fn y => 
            fn z => 
                z >= y andalso y >= z

val t2 = (((sorted3 7) 9) 11)  (* 7 bound to z , 9 bound to y, 11 bound to 11*)

fun sorted3_nicer x y z = z >= y andalso y >= z

(* fold in curray form *)
fun fold f acc xs = (* means fun fold f = fn acc => fn xs =>*)
    case xs of
        [] => acc
       |x::xs' => fold f (f(acc, x)) xs'

(* Partial Application 
- If using partial application to create a polymorphic function,
it may not work due to the value restriction. *)
val incrementAll = List.map(fn x => x + 1)
val removeZero = List.filter(fn x=> x <> 0)

(* Curring warpup *)
fun range(i, j) =
    if i > j
    then []
    else i::range(i+1, j)

fun curry f = fn x => fn y => f (x, y)

fun uncurry f (x, y) = f x y 


val countup2 = (curry range) 1


(*ML has separate mutation 
* Mutable data structures are okay in some situations
    - When "update to state of world" is appropriate model
    - But want most language constructs truly immutable
*)

(*
    - ref e to create a reference with initial contents e of type "t ref"
    - e1 := e2 to update contents
    - !e to retrieve contents 
 *)

val x = ref 42
val y = ref 42
val z = x
val _ = x := 43
val w = (!y) + (!z) (*85*)

(* Callbacks 
- A common idiom: Library takes functions to apply later, when an
event occurs.
*)

(* Library *)
val cbs : (int -> unit) list ref = ref []

fun onKeyEvent f = cbs := f::(!cbs)

fun onEvent i =
    let fun loop fs=
            case fs of
                [] => ()
               |f::fs' => (f i; loop fs')
    in loop (!cbs)
    end

(* Clients 
- Can only register an "int -> unit", so if any other data is needed, must be
in closure's environment
- And if need to "remember" something, need mutable state
*)
val timesPressed = ref 0
val _ = onKeyEvent (fn _ => timesPressed := (!timesPressed) + 1)


fun printIfPressed i=
    onKeyEvent (fn j =>
                    if i = j
                    then print ("you pressed" ^ Int.toString i)
                    else ())
