type suit = Clubs | Diamonds | Hearts | Spades

type rank = Two | Three | Four | Five | Six | Seven | Eight | Nine
          | Ten | Jack | Queen | King | Ace of int

type card = (rank * suit)

type t = card list

let ranks = [Two; Three; Four; Five; Six; Seven; Eight; Nine; Ten;
             Jack; Queen; King; Ace 11]
let suits = [Clubs; Diamonds; Hearts; Spades]

let rank (card:card) : rank = fst card

let suit (card:card) : suit = snd card

let full_deck = ((List.concat (List.map (fun rank -> List.map (fun suit -> (rank, suit)) suits) ranks)))

let shuffle deck =
  let new_deck = List.map (fun card -> (Random.bits (), card)) deck in
  let sort_deck = List.sort compare new_deck in
  List.map (fun weighted_card -> snd weighted_card) sort_deck

let points ((rank, suit):card) : int =
  match rank with
  | Ace x -> x
  | Two -> 2 
  | Three -> 3 
  | Four -> 4 
  | Five -> 5 
  | Six -> 6 
  | Seven -> 7 
  | Eight -> 8 
  | Nine -> 9 
  | Ten -> 10 
  | Jack -> 10 
  | Queen -> 10 
  | King -> 10

let rec reduce_ace (hand:card list) : card list = 
  match hand with
  | [] -> []
  | (Ace 11, suit)::t -> (Ace 1,suit)::t
  | h::t -> h::reduce_ace t

let draw_start (deck:t) = 
  match deck with
  | c1::c2::t -> ([c1; c2], t)
  | d -> ([], d)

let draw deck = 
  match deck with
  | [] -> None
  | h::t -> Some (h, shuffle t)