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
(* Do not use the # character *)
(* Do not need to write down any explicit types*)

fun get_substitutions1(str_lst_lst, str) = 
    case str_lst_lst of
        [] => []
       |head::tail =>let fun non_empty_helper(str_lst_lst, str) =
                            let 
                                val tail_ans = get_substitutions1(tail, str)
                            in
                                case all_except_option(str, head) of
                                    NONE => tail_ans
                                   |SOME v => v @ tail_ans
                            end
                        in
                            non_empty_helper(str_lst_lst, str)
                        end

fun get_substitutions2(str_lst_lst, str) = 
    let fun helper(str_lst_lst, acc) = 
        case str_lst_lst of
            [] => acc
           |head::tail => case all_except_option(str, head) of
                                NONE => helper(tail, acc)
                               |SOME v => helper(tail, acc @ v)
    in
        helper(str_lst_lst, [])
    end

fun similar_names(str_lst_lst, full_name) = 
    let
        val {first=x, middle=y, last=z} = full_name
        val substitutions = get_substitutions2(str_lst_lst, x)
        fun substitute(substitutions) = 
            case substitutions of
                [] => []
               |head::tail => {first=head, last=z, middle=y}::substitute(tail)
    in
        {first=x, last=z, middle=y}::substitute(substitutions)
    end

fun card_color(card) = 
    case card of
        (Spades, _) => Black
       |(Clubs, _) => Black
       |_ => Red

fun card_value(card) =
    case card of
        (_, Ace) => 11
       |(_, Num (i)) => i
       |_ => 10

fun remove_card(cs, c, e) =
    case cs of
        [] => raise e
       |head::tail => if c = head
                         then tail
                         else head::remove_card(tail, c, e)

fun all_same_color(cs) = 
    case cs of
        [] => true
      |_::[] => true
      |head::(neck::rest) => card_color(head) = card_color(neck) andalso all_same_color(neck::rest)

fun sum_cards(cs) =
    let fun sum_helper(cs, acc) = 
            case cs of
                [] => acc
               |head::tail => sum_helper(tail, acc + card_value(head))
    in
        sum_helper(cs, 0)
    end

fun score(held_cards, goal) =
    let 
        val sum = sum_cards(held_cards)
        val preliminary_score = if sum > goal
                                    then 3 * (sum - goal)
                                    else goal - sum
    in
        case all_same_color(held_cards) of
            true => preliminary_score div 2
           |_ => preliminary_score
    end

fun officiate(card_lst, move_lst, goal) =
    let
        fun state(held_card, current_card_lst, current_move_lst) = 
            case current_move_lst of
                [] => held_card
               |head::tail => case head of
                                    Discard c => state(remove_card(held_card, c, IllegalMove), current_card_lst, tail)
                                   |Draw => case current_card_lst of
                                                [] => held_card
                                               |x::xs => let val after_draw_held = x::held_card
                                                           in
                                                               if sum_cards(after_draw_held) > goal
                                                               then after_draw_held
                                                               else state(after_draw_held, xs, tail)
                                                           end
    in
        score(state([], card_lst, move_lst), goal)
    end

(* fun officiate(card_lst, move_lst, goal) =
    let
        fun state(held_card, current_card_lst, current_move_lst) =
            case current_move_lst of
                [] => held_card
               |Discard c::tail => state(remove_card(held_card, c, IllegalMove), current_card_lst, tail)
               |Draw::tail => case current_card_lst of
                                    [] => held_card
                                   |x::xs => let val after_draw_held = x::held_card
                                               in
                                                   if sum_cards(after_draw_held) > goal
                                                   then after_draw_held
                                                   else state(after_draw_held, xs, tail)
                                               end
    in
        score(state([], card_lst, move_lst), goal)
    end *)