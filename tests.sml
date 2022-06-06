use "hw02.sml";

(** Task 1 **)
val a1_test = all_except_option("a", ["b", "a", "c"]);
val a2_test = all_except_option("a", ["b", "c", "d"]);
val a3_test = all_except_option("a", []);

val b_test = get_substitutions1([["Fred","Fredrick"],["Elizabeth","Betty"],["Freddie","Fred","F"]],
"Fred");

val c_test = get_substitutions2([["Fred","Fredrick"],["Elizabeth","Betty"],["Freddie","Fred","F"]],
"Fred");


val d_test = similar_names([["Fred","Fredrick"],["Elizabeth","Betty"],["Freddie","Fred","F"]],
{first="Fred", middle="W", last="Smith"});


(** Task 2 **)
val a_test = card_color((Clubs, King));

val b1_test = card_value((Hearts, Queen));
val b2_test = card_value((Spades, Num 5));
val b3_test = card_value((Diamonds, Ace));

val c1_test = remove_card([(Hearts, Queen), (Spades, Num 3), (Diamonds, Ace)], (Spades, Num 3), IllegalMove);
val c2_test = (remove_card([(Hearts, Queen), (Spades, Num 3), (Diamonds, Ace)], 
                          (Spades, Num 4), IllegalMove)) handle IllegalMove => true;

val d1_test = all_same_color([(Hearts, Num 5), (Hearts, King), (Diamonds, Ace)]);
val d2_test = all_same_color([(Hearts, Num 5), (Spades, King), (Diamonds, Queen)]);

val e_test = sum_cards([(Hearts, Num 5), (Hearts, King), (Diamonds, Ace)]);


val f1_test = score([(Hearts, Num 5), (Hearts, King), (Diamonds, Ace)], 13);
val f2_test = score([(Hearts, Num 5), (Hearts, King), (Diamonds, Ace)], 27);
val f1_test = score([(Hearts, Num 5), (Spades, King), (Diamonds, Ace)], 13);
val f2_test = score([(Hearts, Num 5), (Spades, King), (Diamonds, Ace)], 27);

val g1_test = officiate ([(Hearts, Num 2),(Clubs, Num 4)],[Draw], 15) = 6

val g2_test = officiate ([(Clubs,Ace),(Spades,Ace),(Clubs,Ace),(Spades,Ace)],
                        [Draw,Draw,Draw,Draw,Draw],
                        42)
             = 3

val g3_test = ((officiate([(Clubs,Jack),(Spades,Num(8))],
                         [Draw,Discard(Hearts,Jack)],
                         42);
               false) 
              handle IllegalMove => true)
