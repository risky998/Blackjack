open Player 
open Deck 
open Command
open State

let hit_stay_strategy c player = 
  let value = Player.value_hand player in 
  match value with
  | p when p >= 13 && c <= 6 -> Stay
  | p when p = 12 && c >= 4 && c <= 6 -> Stay
  | p when p = 11 && c <= 10 -> Hit
  | p when p = 10 && c <= 9 -> Hit
  | p when p = 9 && c >= 3 && c <= 5 -> Hit
  | _ -> Hit

let probability_bust player = 
  let value = Player.value_hand player in 
  match value with
  | n when n >= 21 -> 100.0
  | 20 -> 92.0
  | 19 -> 85.0
  | 18 -> 77.0
  | 17 -> 69.0
  | 16 -> 62.0
  | 15 -> 58.0
  | 14 -> 56.0
  | 13 -> 39.0
  | 12 -> 31.0
  |  _ -> 0.0