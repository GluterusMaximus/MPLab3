(* if you use this function to compare two strings (returns true if the same
   string), then you avoid several of the functions in problem 1 having
   polymorphic types that may be confusing *)
fun same_string(s1 : string, s2 : string) =
    s1 = s2

(* put your solutions for problem 1 here *)
(* a *)
fun all_except_option (str : string, strings : string list) =
   let 
      fun spliceString (str : string, []) = []
        | spliceString(str : string, (head : string)::tail) = 
            if same_string(str, head)
            then spliceString(str, tail)
            else head::spliceString(str, tail)
      val spliced = spliceString(str, strings)
   in
      if(spliced = strings) then NONE else SOME(spliced)
   end;   


(* b *)
fun get_substitutions1 ([], s : string ) = []
  | get_substitutions1 ((x : string list)::xs, s : string) =
    case all_except_option(s, x) of
      NONE => get_substitutions1(xs, s)
      | SOME match => match @ get_substitutions1(xs, s);


(* c *)
fun get_substitutions2 (subs : string list list, s : string) =
   let fun helper([], s : string, acc) = acc
         | helper((x : string list)::xs, s : string, acc) =
            case all_except_option(s, x) of
               NONE => helper(xs, s, acc)
               | SOME match => helper(xs, s, acc @ match);
   in
      helper(subs, s, [])
   end;


(* d *)
fun similar_names (name_lists : string list list, name) =
   let 
      fun helper([], name) = []
        | helper((sub : string)::subs', {first=f, middle=m, last=l}) =
            {first=sub, middle=m, last=l}::helper(subs', {first=f, middle=m, last=l})
   in
      case name of
         {first=f, middle, last} => name::helper(get_substitutions2(name_lists, f), name)
   end;


(* you may assume that Num is always used with values 2, 3, ..., 10
   though it will not really come up *)
datatype suit = Clubs | Diamonds | Hearts | Spades
datatype rank = Jack | Queen | King | Ace | Num of int 
type card = suit * rank

datatype color = Red | Black
datatype move = Discard of card | Draw 

exception IllegalMove

(* put your solutions for problem 2 here *)
(* a *)
fun card_color(suit, rank) =
   case suit of
      Clubs => Black
      | Spades => Black
      | Hearts => Red
      | Diamonds => Red;


(* b *)
fun card_value(suit, rank) =
   case rank of
      Num n => n
      | Ace => 11
      | _ => 10;


(* c *)
fun remove_card(cs, c, e) =
   case cs of
      [] => raise e |
      x::xs => if x = c then xs else x::remove_card(xs, c, e);


(* d *)
fun all_same_color(cards) =
   case cards of
      [] => true |
      head::[] => true |
      head::neck::tail => (card_color(head) = card_color(neck)) andalso all_same_color(neck::tail);


(* e *)
fun sum_cards(cards) =
   let fun helper(cards, acc) =
      case cards of
         [] => acc |
         head::tail => helper(tail, card_value(head) + acc)
   in
      helper(cards, 0)
   end;


(* f *)
fun score(cards, goal) =
   let
      val sum = sum_cards(cards)
      val prelim_score = 
         if(sum > goal)
         then 3 * (sum - goal)
         else goal - sum
   in
      if all_same_color(cards)
      then prelim_score div 2
      else prelim_score
   end;


(* g *)
fun officiate(cards, moves, goal) =
   let fun make_move(held_cards, card_list, moves, goal) =
      case moves of
         [] => score(held_cards, goal) |
         move::moves' =>
            case move of
                Discard c => make_move(remove_card(held_cards, c, IllegalMove), card_list, moves', goal) |
                Draw =>
                  case card_list of
                    [] => score(held_cards, goal) |
                    card::card_list' => 
                        if(sum_cards(held_cards) > goal) 
                        then score(held_cards, goal)
                        else make_move(card::held_cards, card_list', moves', goal)
   in
      make_move([], cards, moves, goal)
   end;
