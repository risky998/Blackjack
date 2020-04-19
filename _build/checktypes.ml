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
  val draw_start : t -> card list * t
  val draw : t -> (card * card list) option
end

module DeckCheck : DeckSig = Deck

module type PlayerSig = sig
  type t
  type hand
  val get_value_hand : hand -> int -> int 
  val init_player : string -> int -> t
  val player_hand : t -> hand
  val total_money : t -> int
  val value_hand : t -> int
  val get_id : t -> string
  val is_dealer : t -> bool
  val set_dealer : t -> t
  val bet : int  -> t -> t
  val player_win: t -> t
  val reduce_ace_below_21 : hand -> hand 
  val draw_card : Deck.card -> t -> t
  val draw_card_dealer : Deck.card -> t -> t
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

module type StateSig = sig
  type t 
  type result = Legal of t | Illegal
  val replace_player : Player.t -> Player.t list -> Player.t list
  val first_draw_2 : t -> t
  val init_state : t
end

module StateCheck : StateSig = State

module type AuthorsSig = sig
  val hours_worked : int
end

module AuthorsCheck : AuthorsSig = Authors
