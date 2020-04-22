open Deck
open Player
open Yojson.Basic.Util

type t = {
  players: Player.t list;
  deck: Deck.t
}

type result = Legal of t | Illegal

let rec replace_player p players = 
  match players with
  | [] -> []
  | h::t -> if Player.get_id p = Player.get_id h then p::t
    else h::replace_player p t

let rec get_dealer_hand_value players = 
  match players with 
  | [] -> failwith "Dealer was not found"
  | h::t -> if (is_dealer h) then value_hand h else get_dealer_hand_value t 

let print_cards = function
  | [] -> ""
  | [s] -> s
  | h::t -> List.fold_left (fun acc e -> acc^", "^e) h t

let first_draw_2 g =
  let rec each_player_draw2 g players =
    match players with 
    | [] -> g
    | p::t -> 
      let draw_2 = Deck.draw_start g.deck in
      let cards_2 = fst draw_2 in
      (* ANSITerminal.(print_string [red] ("Cards: ["^
                                        print_cards (List.map(fun card -> string_of_card card) cards_2)^"]\n")); *)
      let remaining_deck = snd draw_2 in
      (* ANSITerminal.(print_string [blue] (Player.get_id p)); *)
      let p' = Player.draw_card (List.nth cards_2 0) p |> Player.draw_card (List.nth cards_2 1) in
      each_player_draw2 {players = replace_player p' g.players;
                         deck = remaining_deck} t in
  each_player_draw2 g g.players

let init_state json = 
  first_draw_2 { players = json |> member "players" |> to_list |> List.map init_player;
                 deck = Deck.(full_deck ()|> shuffle) } 

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

let rec player_won p players = 
  let dealer_value = get_dealer_hand_value players in 
  match players with 
  | [] -> false
  | h::t -> if (Player.get_id p = Player.get_id h) && (dealer_value < value_hand h) then true else false

let rec player_bust p players =  
  match players with 
  | [] -> false
  | h::t -> if (Player.get_id p = Player.get_id h) && (21 < value_hand h) then true else false

let rec player_blackjack p players = 
  match players with 
  | [] -> false
  | h::t -> if (Player.get_id p = Player.get_id h) && (21 = value_hand h) then true else false

let get_player g = 
  let rec get_player_helper players =
    match players with
    | [] -> raise (Failure "no player initiated")
    | h::t -> if (Player.is_ai h) then get_player_helper t
      else h
  in get_player_helper g.players

let get_dealer g = 
  let rec get_dealer_helper players = 
    match players with
    | [] -> raise (Failure "no dealer initiated")
    | h::t -> if not (Player.is_dealer h) then h
      else get_dealer_helper t
  in get_dealer_helper g.players