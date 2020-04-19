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
  val points : rank -> int
end

module DeckCheck : DeckSig = Deck

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
