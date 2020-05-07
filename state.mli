
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

(* The type representing the player's result at the end of the game. *)
type status = PlayerLose | PlayerWin | PlayerBlackJack | PlayerTie

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

(** [double player g] is the updated game after [player] chooses to double. *)
val double: Player.t -> t -> result

(** [stay player g] is [g] after a  player [player] chooses to stay. *)
val stay : Player.t -> t -> t

(** [in_stayed player g] returns whether a player [player] is in ths stayed list of the game *)
val in_stayed : Player.t -> t -> bool

(** [stayed_length g] is the number of players who have already stayed.  *)
val stayed_length : t -> int

val bet : int -> Player.t -> t -> result

(** [all_have_bet g] is true if all non-dealer players have bet, false otherwise. *)
val all_have_bet : t -> bool

(** [get_dealer_hand_value players] gets the dealer's hand value from a list of all the players in state.  *)
val get_dealer_hand_value : Player.t list -> int

(** [game_end_status d p] returns whether the player won, lost, tied, or got a blackjack at the end of the game. *)
val game_end_status: int -> Player.t -> status 

(** [player_won p players] returns whether the player [p] busted, i.e their hand value is higher than 21  *)
val player_bust: Player.t -> bool 

(** [player_won p players] returns whether the player [p] won a hand, i.e their hand value is 21 *)
val player_blackjack: Player.t -> Player.t list -> bool 

val get_player: t -> Player.t

val get_dealer: t -> Player.t

val get_non_dealers: t -> Player.t list

val get_other_players: t -> Player.t list

val dealer_top_card : t -> Deck.card option

(** [reset players] is a new game with each player's money updated based on
    their bets. Every player's hand is also reset to an empty hand. *)
val reset: t->t