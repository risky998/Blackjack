
(** 
   Representation of dynamic adventure state.

   This module represents the state of an adventure as it is being played,
   including the adventurer's current room, the rooms that have been visited,
   and functions that cause the state to change.
*)

(** The abstract type of values representing the game state. *)
type t 

(* The type representing the result of an attempted move. *)
type result = Legal of t | Illegal

val get_players : t -> Player.t list

(** [init_state] is the initial state of the game after each player
    has drawn their first 2 cards and those cards drawn are removed from the 
    deck. *)
val init_state : Yojson.Basic.t -> t

(** [hit player g] is [r] after a  player [player] chooses to hit. 
    If the deck still has available cards to be 
    drawn, then [r] is [Legal g'], i.e. the new state of the game with the player's
    hand and hand value updated, and the drawn card removed from the deck.  
    Otherwise, the result is [Illegal]. *)
val hit : Player.t -> t -> result

val bet : int -> Player.t -> t -> result

(** [player_won p players] returns whether the player [p] won a hand, i.e their hand value is higher than the dealer's hand value  *)
val player_won: Player.t -> Player.t list -> bool 

(** [player_won p players] returns whether the player [p] busted, i.e their hand value is higher than 21  *)
val player_bust: Player.t -> Player.t list -> bool 

(** [player_won p players] returns whether the player [p] won a hand, i.e their hand value is 21 *)
val player_blackjack: Player.t -> Player.t list -> bool 


val get_player: t -> Player.t

val get_dealer: t -> Player.t

val get_other_players: t -> Player.t list

val dealer_info : t -> string * int

val top_card_value : t -> int