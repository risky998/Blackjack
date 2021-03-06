(** 
   Representation of a blackjack player.

   This module handles allowing the player to bet,
   getting the value of a player's hand.
*)

(** The abstract representation type for blackjack player. *)
type t

(** [init_state hand value money] is the initial state of a blackjack player
    In that state the player starts with the cards in [hand], which have
    a value of [value], and has starting money of [money]. *)
val init_player : Yojson.Basic.t -> t

(** [player_hand st] is the hand of the player in state [st]. *)
val player_hand : t -> Deck.card list

(** [total_money st] is the total money of the player in state [st]. *)
val total_money : t -> int

(** [total_money st] is the total money of the player in state [st]. *)
val value_hand : t -> int

(** [get_id st] is the unique identifier of the player in state [st]. *)
val get_id : t -> string

(** [is_ai st] is whether the player is an AI in state [st]. *)
val is_ai : t -> bool

(** [is_dealer st] is whether the player is a dealer in state [st]. *)
val is_dealer : t -> bool

(** [is_dealer st] is whether the player is a split player from an 
    existing player in state [st]. *)
val is_split : t -> bool

(** [set_dealer st] sets the player as a dealer in state [st]. *)
val set_dealer : t -> t

(**  [player_bet money st] is the new state after a player bets [money] on a 
     play. *)
val player_bet : int -> t -> t

(**  [player_double money st] is the new state after a player doubles on a 
     play. *)
val player_double: t -> t 

(** [player_win st] is the new state if the player wins a hand. *)
val player_win: t -> t

(** [player_lose st] is the new state if the player loses a hand. *)
val player_lose: t -> t

(** [player_tie st] is the new state if the player is tied with the dealer's 
    hand. *)
val player_tie: t -> t

(** [player_blackjack st] is the new state if the player has a blackjack 
    hand. *)
val player_blackjack: t -> t

(** [dealer_reset_hand st] is the dealer's hand reset to having no cards. *)
val dealer_reset_hand: t -> t

(** [draw_card card st] is the new state of the player after drawing a 
    card, with the value of Aces reduced from 11 to 1 to ensure the player's
    hand value is below 21 if possible. *)
val draw_card : Deck.card -> t -> t

(** [get_bet st] returns the amount of money bet in state [st]. *)
val get_bet : t -> int

(** [init_temp_player player] is a new temporary player created based on 
    the field existing in player [player] to be used when [player] splits 
    during a hand. *)
val init_temp_player: t -> t

(** [split p] is the updated player [p] after they choose to split,*)
val split: t -> t

(** [split_money_reward money p] is [p] after money [money] has been added to 
    player [p]. *)
val split_money_reward:  int -> t -> t 