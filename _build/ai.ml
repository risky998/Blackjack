open Player 
open Deck 
open Command
open State

let dealer_strategy dealer = 
  let value = Player.value_hand dealer in 
  match value with
  |n when n>=17 -> Stay
  | _ -> Hit

let hit_stay_strategy c player = 
  let value = Player.value_hand player in 
  match value with
  | p when p >= 13 && c <= 6 -> Stay
  | p when p = 12 && c >= 4 && c <= 6 -> Stay
  | p when p = 11 && c <= 10 -> Hit
  | p when p = 10 && c <= 9 -> Hit
  | p when p = 9 && c >= 3 && c <= 5 -> Hit
  | _ -> Hit

(* This code is temporarily unusable until we get the double command working, which will be part of MS2 *)
(* let hit_double_stay_strategy c player = 
   let value = Player.value_hand player in 
   match value with
   | p when p >= 13 && c <= 6 -> Stay
   | p when p = 12 && c >= 4 && c <= 6 -> Stay
   | p when p = 11 && c <= 10 -> Double
   | p when p = 10 && c <= 9 -> Double
   | p when p = 9 && c >= 3 && c <= 5 -> Double
   | _ -> Hit *)

(* More code will be added here to describe the soft strategy for the AI when they have exactly two cards and the dealer's top card is being shown. This will mean that Split and Double need to be functional *)

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