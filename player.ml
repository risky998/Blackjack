open Deck

type hand = card list

type t = {
  id : string;
  player_hand : hand;
  value_hand : int;
  total_money : int;
  player_bet : int;
  dealer: bool;
  ai: bool;
  (* current_turn : bool *)
}

let rec get_value_hand hand acc = 
  match hand with
  | [] -> 0
  | h::t -> get_value_hand t (acc+(Deck.points h))

let init_player id money dealer ai = {
  id = id;
  player_hand = [];
  value_hand = 0;
  total_money = money;
  player_bet = 0;
  dealer = dealer;
  ai = ai;
}

let player_hand st = st.player_hand

let total_money st = st.total_money

let value_hand st = st.value_hand

let get_id st = st.id

let is_dealer st = st.dealer

let set_dealer st = {st with dealer = true}

let bet money st = 
  if (money <= st.total_money) then 
    {st with total_money = st.total_money - money; player_bet = money}
  else raise (Failure "Not enough money")

(* If they player wins, they win twice what they bet *)
let player_win st =  
  {st with total_money = st.total_money + 2*st.player_bet; player_bet = 0}

(* If the player loses, they do not gain back the money that they bet.  *)
let player_lose st =  
  {st with player_bet = 0}

let rec reduce_ace_below_21 hand = 
  let value = get_value_hand hand 0 in
  match value with
  | _ when value > 21 -> reduce_ace_below_21 (Deck.reduce_ace hand)
  | _ -> hand

let draw_card card st = 
  let new_hand = card::st.player_hand in
  let new_value = get_value_hand new_hand 0 in 
  if (new_value > 21) then 
    let reduced_hand = reduce_ace_below_21 new_hand in
    let reduced_value = get_value_hand reduced_hand 0 in
    {st with player_hand = reduced_hand; value_hand = reduced_value}
  else {st with player_hand = new_hand; value_hand = new_value}

let draw_card_dealer card st = 
  let old_value = get_value_hand st.player_hand 0
  in match old_value with 
  | n when n <= 16 -> let new_hand = card::st.player_hand in
    let new_value = get_value_hand new_hand 0 in 
    if (new_value > 21) then 
      let reduced_hand = reduce_ace_below_21 new_hand in
      let reduced_value = get_value_hand reduced_hand 0 in
      {st with player_hand = reduced_hand; value_hand = reduced_value}
    else {st with player_hand = new_hand; value_hand = new_value}
  | _ -> st
(* | _ -> Illegal *)


