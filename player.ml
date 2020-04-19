open Deck

type hand = card list

type t = {
  player_hand : hand;
  value_hand : int;
  total_money : int;
  player_bet : int;
  (* current_turn : bool *)
}

type result = Legal of t | Illegal

let rec get_value_hand hand acc = 
  match hand with
  | [] -> 0
  | h::t -> get_value_hand t (acc+(Deck.points h))

let init_player hand value money = {
  player_hand = hand;
  value_hand = get_value_hand hand 0;
  total_money = money;
  player_bet = 0;
}

let player_hand st = st.player_hand

let total_money st = st.total_money

let value_hand st = st.value_hand

let bet money st = 
  if (money <= st.total_money) then 
    Legal {st with total_money = st.total_money - money; player_bet = money}
  else Illegal

(* If they player wins, they win twice what they bet *)
let player_win st =  
  Legal {st with total_money = st.total_money + 2*st.player_bet; player_bet = 0}

(* If the player loses, they do not gain back the money that they bet.  *)
let player_lose st =  
  Legal {st with player_bet = 0}

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
    Legal {st with player_hand = reduced_hand; value_hand = reduced_value}
  else Legal {st with player_hand = new_hand; value_hand = new_value}


