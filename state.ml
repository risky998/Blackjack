(* Note: You may introduce new code anywhere in this file. *) 
open Deck
open Player
(* TODO: replace [unit] with a type of your own design. *)
type t = {
  players: Player.t list;
  deck: Deck.t
}

let init_state adv =
  failwith "Unimplemented"

let current_room_id st =
  failwith "Unimplemented"

let visited st =
  failwith "Unimplemented"

type result = Legal of t | Illegal

let go ex adv st =
  failwith "Unimplemented"
