
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

(** [replace_player p players] is a new list of players with the new state of
    player p replaced in players.  *)
val replace_player : Player.t -> Player.t list -> Player.t list

(** [first_draw_2 g players] is the new state of the game after each player
    has drawn their first 2 cards and those cards drawn are removed from the 
    deck.  *)
val first_draw_2 : t -> t

(** [init_state] is the initial state of the game after each player
    has drawn their first 2 cards and those cards drawn are removed from the 
    deck. *)
val init_state : t


