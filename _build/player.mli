(** 
   Representation of a blackjack player.

   This module handles allowing the player to bet,
   getting the value of a player's hand.
*)

(* The abstract representation type for a player's hand. *)
type hand

(* The abstract representation type for blackjack player. *)
type t

(** [get_value_hand hand acc] is the value of the cards in a player's hand. *)
val get_value_hand : hand -> int -> int 

(** [init_state hand value money] is the initial state of a blackjack player
    In that state the player starts with the cards in [hand], which have
    a value of [value], and has starting money of [money]. *)
val init_player : string -> int -> t

(** [player_hand st] is the hand of the player in state [st]. *)
val player_hand : t -> hand

(** [total_money st] is the total money of the player in state [st]. *)
val total_money : t -> int

(** [total_money st] is the total money of the player in state [st]. *)
val value_hand : t -> int

(** [get_id st] is the unique identifier of the player in state [st]. *)
val get_id : t -> string

(** [is_deaer st] is whether the player is a dealer in state [st]. *)
val is_dealer : t -> bool

(** [set_dealer st] is sets the player as a dealer in state [st]. *)
val set_dealer : t -> t

(** [bet money st] is the new state after a player bets [money] on a play. *)
val bet : int  -> t -> t

(** [player_win st] is the new state if the player wins a hand *)
val player_win: t -> t

(** [reduce_ace_below_21 hand] is the new hand of the player after changing 
    the values of as few Aces as possible so that the value of the new hand is 
    less than 21 if possible. *)
val reduce_ace_below_21 : hand -> hand 

(** [draw_card card st] is the new state of the player after drawing a 
    card, with the value of Aces reduced from 11 to 1 to ensure the player's
    hand value is below 21 if possible. *)
val draw_card : Deck.card -> t -> t

(** [draw_card card st] is the new state of the dealer after drawing a 
    card. The dealer will only draw if hand is less than or equal 16 in value.  *)
val draw_card_dealer : Deck.card -> t -> t
