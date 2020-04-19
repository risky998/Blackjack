(** 
   Representation of a card deck with 52 cards.

   This modile handles shuffling the deck.
*)

(*The abstract representation type for a deck*)
type t

(*The type representing the suit of a card*)
type suit

(*The type that represents the value of a card*)
type rank

(*The type that represents a card*)
type card

(*[ranks] is a list of all the possible card ranks*)
val ranks : rank list

(*[suits] is a list of all the possible card suits*)
val suits : suit list

(*[rank card] is the number value of [card]*)
val rank : card -> rank

(*[suit card] is the suit of [card]*)
val suit : card -> suit

(*[full_deck] is a full deck of 52 cards*)
val full_deck : t

(*[shuffle deck] is a deck of 52 cards with the order randomized*)
val shuffle : t -> t

(*[points] is the blackjack value of each card*)
val points : rank -> int