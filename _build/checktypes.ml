module type DeckSig = sig
  type t
  type suit
  type rank
  type card
  val ranks : rank list
  val suits : suit list
  val rank : card -> rank
  val suit : card -> suit
  val full_deck : t
  val shuffle : t -> t
  val points : card -> int
  val reduce_ace : card list -> card list
end

module DeckCheck : DeckSig = Deck

module type PlayerSig = sig
  type t
  type hand
  type result = Legal of t | Illegal
  val get_value_hand : hand -> int -> int 
  val init_player : hand -> int -> int -> t
  val player_hand : t -> hand
  val total_money : t -> int
  val value_hand : t -> int
  val bet : int -> t -> result
  val reduce_ace_below_21 : hand -> hand 
  val draw_card : Deck.card -> t -> result
end

module PlayerCheck : PlayerSig = Player

module type CommandSig = sig
  type money = int
  type command = 
    | Quit 
    | Bet of money
    | Stay
    | Hit
    | Money
    | Help
  exception Empty
  exception Malformed
  val parse : string -> command
end

module CommandCheck : CommandSig = Command

(* module type StateSig = sig
   type t 
   val init_state : Adventure.t -> t
   val current_room_id : t -> string
   val visited : t -> string list
   type result = Legal of t | Illegal
   val go : Adventure.exit_name -> Adventure.t -> t -> result
   end

   module StateCheck : StateSig = State *)

module type AuthorsSig = sig
  val hours_worked : int
end

module AuthorsCheck : AuthorsSig = Authors
