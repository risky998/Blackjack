(** 
   This module has a number of helper functions to deal with blackjack 
   strategy for both the players and the AI units in the game
*)

(** [dealer_strategy] is the action a [dealer] must take given the current value of their hand *)
val dealer_strategy: Player.t-> Command.command

(* (** [hit_stay_strategy c player] is the ideal strategy for a [player]
    when their only two command options are hit and stay and the deale's top card has a value [c]*)
   val hit_stay_strategy: int->Player.t-> Command.command *)

(** [hit_double_stay_strategy c player] is the ideal strategy for a [player]
    when their only three command options are hit and stay and double and the deale's top card has a value [c]*)
val hit_double_stay_strategy: int->Player.t-> Command.command

(** [probability_bust player] is the probability that the [player] will go bust if they draw a card with the value of their current hand *)
val probability_bust: Player.t-> float

