(* Note: You may introduce new code anywhere in this file. *) 
open Deck
open Player
(* TODO: replace [unit] with a type of your own design. *)
type t = {
  players: Player.t list;
  deck: Deck.t
}

type result = Legal of t | Illegal

let rec replace_player p players = 
  match players with
  | [] -> []
  | h::t -> if Player.get_id p = Player.get_id h then p::t
    else replace_player p t

let first_draw_2 g =
  let rec each_player_draw2 g players =
    match players with 
    | [] -> g
    | p::t -> 
      let draw_2 = Deck.draw_start g.deck in
      let cards_2 = fst draw_2 in
      let remaining_deck = snd draw_2 in
      let p' = Player.draw_card (List.nth cards_2 0) p |> Player.draw_card (List.nth cards_2 1) in
      each_player_draw2 {players = replace_player p' g.players;
                         deck = remaining_deck} t in
  each_player_draw2 g g.players

let init_state = 
  first_draw_2 { players = [Player.init_player "1" 300;
                            Player.init_player "2" 300; 
                            Player.init_player "3" 300;
                            Player.init_player "4" 300];
                 deck = Deck.(full_deck |> shuffle) }


let rec update_player card p players = 
  match players with 
  | [] -> []
  | h::t -> if p = h then (Player.draw_card card p)::t
    else h::(update_player card p t) 

let rec hit player g =
  match Deck.draw g.deck with
  | None -> Illegal
  | Some (card, remaining) -> 
    Legal {
      players = update_player card player g.players;
      deck = remaining
    }