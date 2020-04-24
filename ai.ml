open Player 
open Deck 
open Command
open State

let hit_stay_strategy c p = 
  let value = Player.value_hand p in 
  match value with
  | p when p >= 13 && c <= 6 -> Stay
  | p when p = 12 && c >= 4 && c <= 6 -> Stay
  | p when p = 11 && c <= 10 -> Hit
  | p when p = 10 && c <= 9 -> Hit
  | p when p = 9 && c >= 3 && c <= 5 -> Hit
  | _ -> Hit

