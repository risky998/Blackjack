(** 
   This module has a number of helper functions to deal with blackjack 
   strategy for both the players and the AI units in the game
*)

val hit_stay_strategy: int->Player.t-> Command.command
