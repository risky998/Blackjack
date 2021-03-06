open Deck
open Yojson.Basic.Util


type t = {
  id : string;
  player_hand : Deck.card list;
  value_hand : int;
  total_money : int;
  player_bet : int;
  dealer: bool;
  ai: bool;
  split: bool
}

(** [get_value_hand hand acc] is the value of the cards in a player's hand. *)
let rec get_value_hand hand acc = 
  match hand with
  | [] -> acc
  | h::t -> get_value_hand t (acc+(Deck.points h))

(** [init_player j] is type [player] with the fields id, player_hand, 
    value_hand, total_money, player_bet, dealer, and ai.
    Requires: [j] is a valid JSON player representation. *)
let init_player j = {
  id = j |> member "id" |> to_string;
  player_hand = [];
  value_hand = 0;
  total_money = j |> member "total_money" |> to_int;
  player_bet = 0;
  dealer = j |> member "dealer" |> to_bool;
  ai = j |> member "ai" |> to_bool;
  split = j |> member "split" |> to_bool;
}

let player_hand st = st.player_hand

let total_money st = st.total_money

let value_hand st = st.value_hand

let get_id st = st.id

let is_ai st = st.ai

let is_dealer st = st.dealer

let get_bet st = st.player_bet

let is_split st = st.split

let set_dealer st = {st with dealer = true}

let player_bet money st = 
  {st with total_money = st.total_money - money; player_bet = money}

let player_double st = 
  let bet = st.player_bet in
  if 2*bet > st.total_money then 
    {
      st with total_money = 0; 
              player_bet = bet + st.total_money
    }
  else
    {
      st with total_money = st.total_money - bet; 
              player_bet = 2*bet
    }

let player_win st =  
  let float_money = (float_of_int st.total_money) +. 
                    ((float_of_int st.player_bet) *. 1.5) in
  let int_money = int_of_float float_money in
  {st with total_money = int_money; 
           player_bet = 0; player_hand = []; value_hand = 0}

let player_lose st =  
  {st with player_bet = 0; player_hand = []; value_hand = 0}

let player_tie st =  
  {st with total_money = st.total_money + st.player_bet; 
           player_bet = 0; player_hand = []; value_hand = 0}

let player_blackjack st =  
  {st with total_money = st.total_money + 2*st.player_bet; 
           player_bet = 0; player_hand = []; value_hand = 0}

let dealer_reset_hand st =  
  {st with player_hand = []; value_hand = 0}

(** [reduce_ace_below_21 hand] is the new hand of the player after changing 
    the values of as few Aces as possible so that the value of the new hand is 
    less than 21 if possible. *)
let rec reduce_ace_below_21 hand = 
  let value = get_value_hand hand 0 in
  if value > 21 then 
    let new_hand = Deck.reduce_ace hand in
    match get_value_hand (new_hand) 0 with
    | n when n = value -> hand
    | _ -> new_hand
  else hand

let draw_card card st = 
  let new_hand = card::st.player_hand in
  let new_value = get_value_hand new_hand 0 in 
  if (new_value > 21) then 
    let reduced_hand = reduce_ace_below_21 new_hand in
    let reduced_value = get_value_hand reduced_hand 0 in
    {st with player_hand = reduced_hand; value_hand = reduced_value}
  else {st with player_hand = new_hand; value_hand = new_value;}

let init_temp_player p =
  let lst = p.player_hand in 
  let card2 = List.nth lst 1 in
  let value = points card2 in
  match rank card2 with
  | Ace x -> 
    {p with id = get_id p^"(Split)";
            player_hand = [(Ace 11, (suit card2))];
            value_hand = 11;
            total_money = 0;
            split = true;}
  | _ -> 
    {p with id = get_id p^"(Split)";
            player_hand = [card2];
            value_hand = value;
            total_money = 0;
            split = true;}

let split p = 
  let card1 = List.nth p.player_hand 0 in 
  match rank card1 with
  | Ace x -> 
    { p with  player_hand = [(Ace 11, (suit card1))];
              value_hand = 11;
              total_money = p.total_money - p.player_bet}

  | _ -> 
    { p with  player_hand = [card1];
              value_hand = p.value_hand/2;
              total_money = p.total_money - p.player_bet}

let split_money_reward money p = 
  {p with total_money = p.total_money + money}