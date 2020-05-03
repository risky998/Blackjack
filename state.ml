open Deck
open Player
open Yojson.Basic.Util

type t = {
  players: Player.t list;
  deck: Deck.t;
  stayed: string list;
}

type result = Legal of t | Illegal

let get_players g = g.players

(** [get_dealer_hand_value players] gets the dealer's hand value from a list of all the players in state.  *)
let rec get_dealer_hand_value players = 
  match players with 
  | [] -> failwith "Dealer was not found"
  | h::t -> if (is_dealer h) then value_hand h else get_dealer_hand_value t 

let print_cards = function
  | [] -> ""
  | [s] -> s
  | h::t -> List.fold_left (fun acc e -> acc^", "^e) h t

let init_state json = 
  { players = json |> member "players" |> to_list |> List.map init_player;
    deck = Deck.(full_deck () |> shuffle);
    stayed = [] } 

(** [update_player card p players] is a new list of players after a player [p]
    has drawn a card [card] and his corresponding hand and hand value are 
    updated in the new player list. *)
let rec update_player card p players = 
  match players with 
  | [] -> []
  | h::t -> if p = h then (Player.draw_card card p)::t
    else h::(update_player card p t) 

(** [double_player p players] is a new list of players after a player [p]
    has doubled on a play *)
let rec double_player p players = 
  match players with 
  | [] -> []
  | h::t -> if p = h then (Player.player_double p)::t
    else h::(double_player p t) 

let stay player g =
  { g with stayed = (get_id player)::g.stayed;}

let in_stayed player game = 
  let rec in_stayed_helper player lst = 
    match lst with 
    | [] -> false
    | h::t -> if (get_id player = h) then true else in_stayed_helper player t
  in in_stayed_helper player (game.stayed) 

let stayed_length game = 
  List.length (game.stayed)

(** DOCUMENTATION*)
let rec find_player_hand_value p players = 
  match players with
  | [] -> 0
  | h::t -> if p = get_id h then value_hand h else find_player_hand_value p t

let rec hit player g =
  match Deck.draw g.deck with
  | None -> Illegal
  | Some (card, remaining) -> 
    let new_players = update_player card player g.players in 
    let hand = find_player_hand_value (get_id player) new_players in
    Legal {
      players = new_players;
      deck = remaining;
      stayed = if (hand > 21) then (get_id player)::g.stayed else g.stayed
    }

(** [replace_player p players] is a new list of players with the new state of
    player p replaced in players.  *)
let rec replace_player p players = 
  match players with
  | [] -> []
  | h::t -> if Player.get_id p = Player.get_id h then p::t
    else h::replace_player p t

(* let turn st stays= 
   if stays is 4 then return st
   otherwise -> call dealer function 
   call ai function 
   prompt player for action + match player action
*)


let bet money player g = 
  let player_money = total_money player in
  if (money <= player_money) then 
    let new_player_bet = player_bet money player in
    let draw_2 = Deck.draw_start g.deck in
    let cards_2 = fst draw_2 in
    let remaining_deck = snd draw_2 in
    let new_player_draw = Player.draw_card (List.nth cards_2 0) new_player_bet |> 
                          Player.draw_card (List.nth cards_2 1) in
    let new_players = replace_player new_player_draw g.players in
    Legal {g with
           players = new_players;
           deck = remaining_deck;
          }
  else Illegal

(* let next_turn players g =  *)

let rec player_won dealer_value p players = 
  (* let dealer_value = get_dealer_hand_value players in  *)
  match players with 
  | [] -> false
  | h::t -> if (Player.get_id p = Player.get_id h) && (dealer_value < value_hand h) then true else player_won dealer_value p t

(* let rec player_bust p players =  
   match players with 
   | [] -> false
   | h::t -> if (Player.get_id p = Player.get_id h) && (21 < value_hand h) then true else false *)

let player_bust player = 
  if value_hand player > 21 then true else false

let rec player_blackjack p players = 
  match players with 
  | [] -> false
  | h::t -> if (Player.get_id p = Player.get_id h) && (21 = value_hand h) then true else false

let get_player g = 
  let rec get_player_helper players =
    match players with
    | [] -> raise (Failure "no player initiated")
    | h::t -> if (is_ai h) then get_player_helper t
      else h
  in get_player_helper g.players

let get_other_players g =
  let rec get_others_helper players =
    match players with
    | [] -> []
    | h::t -> if (is_ai h && not (is_dealer h)) then h::get_others_helper t
      else get_others_helper t
  in get_others_helper g.players

let get_dealer g = 
  let rec get_dealer_helper players = 
    match players with
    | [] -> raise (Failure "no dealer initiated")
    | h::t -> if not (is_dealer h) then h
      else get_dealer_helper t
  in get_dealer_helper g.players

let dealer_info g = 
  let dealer = get_dealer g in
  let dealer_hand = player_hand dealer in
  let dealer_len = List.length dealer_hand in
  if dealer_len = 0 then ("", 0) else (string_of_card (List.nth dealer_hand (dealer_len-1)), dealer_len)

let top_card_value g =
  let dealer = get_dealer g in
  let dealer_hand = player_hand dealer in
  let dealer_len = List.length dealer_hand in
  if dealer_len = 0 then 0 else Deck.points (List.nth dealer_hand (dealer_len-1)) 

let reset game = 
  let dealer_value = get_dealer_hand_value game.players in 
  let rec reward_reset_state players= 
    match players with 
    |[] -> players 
    | h::t -> if not (is_dealer h) then 
        if (player_won dealer_value h players) then player_win h :: (reward_reset_state t) else player_lose h :: (reward_reset_state t)
      else h::reward_reset_state t
  in {players = reward_reset_state game.players; deck = Deck.(full_deck () |> shuffle); stayed = []}