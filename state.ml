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

let rec get_dealer_hand_value players = 
  match players with 
  | [] -> failwith "Dealer was not found"
  | h::t -> if (is_dealer h) then value_hand h else get_dealer_hand_value t 

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
    | h::t -> if is_dealer h then h
      else get_dealer_helper t
  in get_dealer_helper g.players

let get_non_dealers g = 
  let rec get_non_dealer_helper players =
    match players with
    | [] -> []
    | h::t -> if (is_dealer h) then get_non_dealer_helper t
      else h::get_non_dealer_helper t
  in get_non_dealer_helper g.players

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
  | h::t -> if get_id p = get_id h then (Player.draw_card card h)::t
    else h::(update_player card p t) 

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

(** [find_player_hand_value] finds the player [p] in the list of [players]
    and gets their hand value. *)
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
    ANSITerminal.(print_string [yellow] 
                    ("\nDrew "^string_of_card card^"\n"));
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

(** [all_have_bet players] is true if all non-dealer players have bet, 
    false otherwise. *)
let all_have_bet players = List.fold_left (fun acc p -> 
    if (not (is_dealer p)) 
    then acc && get_bet p <> 0 else acc) true players

(** [draw_two_cards player deck] is a tuple where the first element is the 
    new player after drawing two cards and the second element is the 
    remaining deck after two cards have been removed. *)
let draw_two_cards player deck =
  let draw_2 = Deck.draw_start deck in
  let cards_2 = fst draw_2 in
  let remaining_deck = snd draw_2 in
  let new_player_draw = Player.draw_card (List.nth cards_2 0) player |> 
                        Player.draw_card (List.nth cards_2 1) in
  (new_player_draw, remaining_deck)

let bet money player g = 
  let player_money = total_money player in
  if (money <= player_money && money > 0) then 
    let new_player_bet = player_bet money player in
    let result = draw_two_cards new_player_bet g.deck in
    let new_player_drew = fst result in
    let remaining_deck = snd result in 
    let new_players = replace_player new_player_drew g.players in
    if all_have_bet new_players then 
      let result = draw_two_cards (get_dealer g) remaining_deck in
      let dealer_drew = fst result in
      let dealer_drew_deck = snd result in 
      let new_players' = replace_player dealer_drew new_players in
      Legal { g with
              players = new_players';
              deck = dealer_drew_deck;
            }
    else 
      Legal { g with
              players = new_players;
              deck = remaining_deck;
            }
  else Illegal

(** [double_helper p players] is a new list of players after a player [p]
    has doubled on a play *)
let rec double_helper p players = 
  match players with 
  | [] -> []
  | h::t -> if get_id p = get_id h then 
      (Player.player_double p)::t
    else h::(double_helper p t) 

let double player g =
  if total_money player = 0 then Illegal 
  else
    let double_player = double_helper player g.players in 
    match Deck.draw g.deck with
    | None -> Illegal
    | Some (card, remaining) -> 
      let new_players = update_player card player double_player in 
      ANSITerminal.(print_string [yellow] 
                      ("\nDrew "^string_of_card card^"\n"));
      Legal { 
        players = new_players; 
        deck = remaining;
        stayed = (get_id player)::g.stayed
      }

let dealer_top_card g = 
  let dealer = get_dealer g in
  let dealer_hand = player_hand dealer in
  let dealer_len = List.length dealer_hand in
  if dealer_len = 0 then None else Some (List.nth dealer_hand (dealer_len-1))

(** [split_helper p players] is a new list of players after a player [p]
    has split on a play *)
let rec split_helper p players = 
  let temp = init_temp_player p in
  let players = 
    match players with 
    | [] -> []
    | h::t -> if get_id p = get_id h then 
        (Player.split p)::t
      else h::(split_helper p t) 
  in players@[temp]

let split player g = 
  if Deck.can_split_pair (player_hand player) then
    let new_players = split_helper player g.players in
    Legal {g with players = new_players}
  else Illegal

type status = PlayerLose | PlayerWin | PlayerBlackJack | PlayerTie

(** [game_end_status dealer_value p] is the status [status] of player [p] in the game given the value [dealer_value] of the dealer's hand.*)
let game_end_status dealer_value p = 
  let p_val = value_hand p in
  let p_hand = List.length (player_hand p) in 
  if p_hand = 2 && p_val = 21 then PlayerBlackJack
  else 
    match (dealer_value, p_val) with
    | (d,v) when v > 21 -> PlayerLose
    | (d,v) when d > 21 && v < 21 -> PlayerWin
    | (d,v) when d > 21 && v = 21 -> PlayerWin
    | (d,v) when d < v && v = 21 -> PlayerWin
    | (d,v) when v > d -> PlayerWin
    | (d,v) when d = v -> PlayerTie
    | (d,v) when v < d -> PlayerLose
    | _ -> raise (Failure "Game Error")

(** [reset_dealer players] is a new Player list with the dealer's
    hand set to an empty hand. *)
let rec reset_dealer players =
  match players with
  | [] -> raise (Failure "no dealer initiated")
  | h::t -> if is_dealer h then dealer_reset_hand h::t
    else h::reset_dealer t

(** [get_player_by_id id players] returns [Some player] 
    if a player exists with id [id] else is [None] *)
let rec get_player_by_id id players = 
  match players with 
  |[] -> None 
  |h::t -> if (get_id h = id) then Some h else get_player_by_id id t

(** [remove_player_by_id id players] is a Player list [players]
    without the player with id [id]. *)
let rec remove_player_by_id id players = 
  match players with  
  |[] -> [] 
  |h::t -> if (get_id h = id) then t else h::remove_player_by_id id t

(** [update_split players] is the new list of players [players] after 
    players who split have been dealt their appropriate rewards from their 
    split hands and the extraneous split hands have been removed.*)
let rec update_split players = 
  match players with 
  | [] -> players
  | h::t -> let id = get_id h in 
    begin match get_player_by_id (id ^ "(Split)") t with 
      | None -> h::update_split t
      | Some p -> split_money_reward (total_money p) h ::
                  update_split (remove_player_by_id (id^"(Split)") t)
    end

let reset game = 
  let dealer_value = get_dealer_hand_value game.players in 
  let rec reward_reset_state players= 
    match players with 
    |[] -> players 
    | h::t -> if not (is_dealer h) then 
        begin 
          match game_end_status dealer_value h with
          | PlayerBlackJack -> Player.player_blackjack h::(reward_reset_state t) 
          | PlayerTie -> Player.player_tie h::(reward_reset_state t) 
          | PlayerWin -> Player.player_win h::(reward_reset_state t) 
          | PlayerLose -> 
            let player_lose = Player.player_lose h in
            if (total_money player_lose) <= 0 && (is_ai h) 
            then 
              begin
                ANSITerminal.(print_string [blue] 
                                ("\nPlayer "^(get_id player_lose)^
                                 " ran out of money."));
                reward_reset_state t 
              end
            else player_lose::reward_reset_state t
        end
      else h::reward_reset_state t
  in 
  let reset_players = reward_reset_state game.players in
  let reset_dealer = reset_dealer reset_players in
  let split_players = update_split reset_dealer in 
  {players = split_players; deck = Deck.(full_deck () |> shuffle); stayed = []}