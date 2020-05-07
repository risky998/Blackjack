module type DeckSig = sig
  type t
  type suit = Clubs | Diamonds | Hearts | Spades
  type rank = Two | Three | Four | Five | Six | Seven | Eight | Nine
            | Ten | Jack | Queen | King | Ace of int
  type card = (rank * suit)
  val ranks : rank list
  val suits : suit list
  val rank : card -> rank
  val suit : card -> suit
  val empty : t
  val size: t -> int
  val full_deck : unit -> t
  val shuffle : t -> t
  val points : card -> int
  val reduce_ace : card list -> card list 
  val draw_start : t -> card list * t
  val draw : t -> (card * t) option
  val string_of_card : card -> string
end

module DeckCheck : DeckSig = Deck

module type PlayerSig = sig
  type t
  val init_player : Yojson.Basic.t -> t
  val player_hand : t -> Deck.card list
  val total_money : t -> int
  val value_hand : t -> int
  val get_id : t -> string
  val is_ai : t -> bool
  val is_dealer : t -> bool
  val get_bet : t -> int
  val is_current_turn : t -> bool
  val set_dealer : t -> t
  val player_bet: int -> t -> t
  val player_win: t -> t
  val player_lose: t -> t
  val player_tie: t -> t
  val player_blackjack: t -> t
  val dealer_reset_hand: t -> t
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
    | Double
    | Hit
    | Help
  exception Empty
  exception Malformed
  val parse : string -> command
end

module CommandCheck : CommandSig = Command

module type StateSig = sig
  type t 
  type result = Legal of t | Illegal
  type status = PlayerLose | PlayerWin | PlayerBlackJack | PlayerTie
  val get_players : t -> Player.t list
  val init_state : Yojson.Basic.t -> t
  val hit : Player.t -> t -> result
  val bet : int -> Player.t -> t -> result
  val all_have_bet : t -> bool
  val get_dealer_hand_value : Player.t list -> int
  val stay: Player.t -> t -> t
  val in_stayed: Player.t -> t -> bool
  val stayed_length: t -> int 
  val game_end_status: int -> Player.t -> status 
  val player_bust: Player.t -> bool 
  val player_blackjack: Player.t -> Player.t list -> bool 
  val get_player: t -> Player.t
  val get_dealer: t -> Player.t
  val get_other_players: t -> Player.t list
  val get_non_dealers: t -> Player.t list
  val dealer_top_card : t -> Deck.card option
end

module StateCheck : StateSig = State

module type AiSig = sig
  val dealer_strategy: Player.t -> Command.command
  val hit_stay_strategy: int->Player.t-> Command.command
  val probability_bust: Player.t-> float
end

module AiCheck : AiSig = Ai

module type AuthorsSig = sig
  val hours_worked : int
end

module AuthorsCheck : AuthorsSig = Authors
