(**
   Representation of a card deck with 52 cards.

   This module handles shuffling the deck.
*)

(** The abstract representation type for a deck. *)
type t

(** The type representing the suit of a card. *)
type suit = Clubs | Diamonds | Hearts | Spades

(** The type that represents the value of a card *)
type rank = Two | Three | Four | Five | Six | Seven | Eight | Nine
          | Ten | Jack | Queen | King | Ace of int

(** The type that represents a card *)
type card = (rank * suit)

(** [ranks] is a list of all the possible card ranks *)
val ranks : rank list

(** [suits] is a list of all the possible card suits *)
val suits : suit list

(** [rank card] is the number value of [card] *)
val rank : card -> rank

(** [suit card] is the suit of [card] *)
val suit : card -> suit

(** [size] is the size of the deck [t] *)
val size : t -> int

(** [empty] is an empty deck [t] *)
val empty : t

(** [full_deck] is a full deck of 52 cards *)
val full_deck : unit -> t

(** [shuffle deck] is a deck of 52 cards with the order randomized*)
val shuffle : t -> t

(** [points] is the blackjack value of each card*)
val points : card -> int

(** [reduce_ace] is the new hand with the first Ace of value 11 
    reduced to a value of 1 so that the player does not bust. *)
val reduce_ace : card list -> card list 

(** [draw_start deck] is a tuple where the first element is a list of the 2
     cards drawn from the deck and the second element is the new deck after the 
     2 cards have been removed. *)
val draw_start : t -> card list * t

(** [draw_start deck] is Some (card, deck') where card is the card drawn
     from the deck and deck' is the reamining deck which is shuffled.
     None is returned if there are no more cards to be drawn from the deck. *)
val draw : t -> (card * t) option

(** [string_of_card card] is a card in string form, for example, (Two, Hearts) 
     is "Two of Hearts" and (King, Spades) is "King of Spades". *)
val string_of_card : card -> string

(** [can_split_pair hand] is true if a hand can be split, that is the hand
     must contain only 2 cards and the 2 cards should be of the same rank. *)
val can_split_pair : card list -> bool