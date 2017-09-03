(* Dan Grossman, Coursera PL, HW2 Provided Code *)

(* if you use this function to compare two strings (returns true if the same
   string), then you avoid several of the functions in problem 1 having
   polymorphic types that may be confusing *)
fun same_string(s1 : string, s2 : string) =
    s1 = s2

(* put your solutions for problem 1 here *)

(* return NONE if the string is not in the list, else return SOME lst where lst is
identical to the argument list except the string is not in it. *)

fun all_except_option (str, str_lst) =
    case str_lst of
        [] => NONE
      | head::tail => if same_string(head, str)
                        then SOME tail
                        else case all_except_option(str, tail) of
                               NONE => NONE
                             | SOME xs => SOME (head::xs)
                             

(* you may assume that Num is always used with values 2, 3, ..., 10
   though it will not really come up *)
datatype suit = Clubs | Diamonds | Hearts | Spades
datatype rank = Jack | Queen | King | Ace | Num of int
type card = suit * rank

datatype color = Red | Black
datatype move = Discard of card | Draw

exception IllegalMove

(* put your solutions for problem 2 here *)
(* Do not use isSome, valOf, null, hd, tl in this homework *)

