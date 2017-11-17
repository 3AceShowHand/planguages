(* Coursera Programming Languages, Homework 3, Provided Code *)

exception NoAnswer

datatype pattern = Wildcard
		 | Variable of string
		 | UnitP
		 | ConstP of int
		 | TupleP of pattern list
		 | ConstructorP of string * pattern

datatype valu = Const of int
	      | Unit
	      | Tuple of valu list
	      | Constructor of string * valu

fun g f1 f2 p =
    let 
	val r = g f1 f2 
    in
	case p of
	    Wildcard          => f1 ()
	  | Variable x        => f2 x
	  | TupleP ps         => List.foldl (fn (p,i) => (r p) + i) 0 ps
	  | ConstructorP(_,p) => r p
	  | _                 => 0
    end

(**** for the challenge problem only ****)

datatype typ = Anything
	     | UnitT
	     | IntT
	     | TupleT of typ list
	     | Datatype of string

(**** you can put all your code here ****)

fun only_capitals (lst) =
	List.filter (fn x => Char.isUpper(String.sub(x, 0))) lst

fun longest_string1 (lst) =
	List.foldl (fn (x, y) => if String.size(x) > String.size(y) then x else y) "" lst

fun longest_string2 (lst) =
	List.foldl (fn (x, y) => if String.size(x) >= String.size(y) then x else y) "" lst

fun longest_string_helper f lst =
	List.foldl (fn(x, y) => if f(String.size(x), String.size(y)) then x else y) "" lst

val longest_string3 = longest_string_helper (fn (x, y) => x > y)

val longest_string4 = longest_string_helper (fn (x, y) => x >= y)

val sqrt_of_abs = Math.sqrt o Real.fromInt o abs

val longest_capitalized =
	fn lst => (longest_string1 o only_capitals) lst

val rev_string =
	fn str => (String.implode o List.rev o String.explode) str

fun first_answer f lst =
	case lst of
		[] => raise NoAnswer
	   |x::xs => case f(x) of
	   					SOME v => v
					   |_ => first_answer f xs

(* all_answers of type ('a -> 'b list option) -> 'a list -> 'b list option
(notice the 2 arguments are curried). The first argument should be applied to elements of the second
argument. If it returns NONE for any element, then the result for all_answers is NONE. Else the
calls to the first argument will have produced SOME lst1, SOME lst2, ... SOME lstn and the result of
all_answers is SOME lst where lst is lst1, lst2, ..., lstn appended together (order doesn't matter). *)

fun all_answers f lst =
	let fun loop (lst, acc) =
		case lst of
			[] => SOME acc
		   |x::xs => case f(x) of
		   					NONE => NONE
                           |SOME v => loop (xs, acc @ v)
	in
		loop (lst, [])
	end

fun count_wildcards p =
	let
		val f1 = fn () => 1	(* will be called by all pattern except Variable *)
		val f2 = fn x => 0
	in
		g f1 f2 p
	end

fun count_wild_and_variable_lengths p =
	let
		val count = count_wildcards p
		val f1 = fn () => 0
		val f2 = fn x => String.size(x)
	in
		count + (g f1 f2 p)
	end

fun count_some_var (str, p) =
	let
		val f1 = fn () => 0
		val f2 = fn x => if x = str then 1 else 0
	in
		g f1 f2 p
	end

fun check_pat p =
	let 
		fun get_var_names p =
			case p of
				Wildcard => []
				|Variable (x) => [x]
				|TupleP p => List.foldl (fn (sub, prev) => (get_var_names sub) @ prev) [] p
				|ConstructorP(str, pat) => get_var_names pat
				|_ => []

		fun is_repeat lst = 
			case lst of
				[] => true
				|x::xs => if List.exists (fn str => x = str) xs then false else is_repeat xs
	in
		is_repeat (get_var_names p)
	end

fun match (value, pattern) =
	case (value, pattern) of
		(_, Wildcard) => SOME []
	   |(value, Variable(str)) => SOME [(str, value)]
	   |(Unit, UnitP) => SOME []
	   |(Const x, ConstP y) => if x = y then SOME [] else NONE
	   |(Constructor(str1, v), ConstructorP(str2, p)) => if str1 = str2 then match(v, p) else NONE
	   |(Tuple vl, TupleP pl) => if List.length vl = List.length pl
	   								 then let val pairs = ListPair.zip(vl, pl)
										   in
										      case all_answers match pairs of
											  	SOME v => SOME v
											   |_ => NONE
											end
									 else NONE
		|_ => NONE

fun first_match value lst =
	SOME (first_answer (fn p => match(value, p)) lst)
	handle NoAnswer => NONE 