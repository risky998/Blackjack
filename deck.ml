type suit = Clubs | Diamonds | Hearts | Spades

type rank = Two | Three | Four | Five | Six | Seven | Eight | Nine
          | Ten | Jack | Queen | King | Ace 

type card = (rank * suit)

type t = card list

let ranks = [Two; Three; Four; Five; Six; Seven; Eight; Nine; Ten;
             Jack; Queen; King; Ace]
let suits = [Clubs; Diamonds; Hearts; Spades]

let rank (card:card) : rank = fst card

let suit (card:card) : suit = snd card

let full_deck = ((List.concat (List.map (fun rank -> List.map (fun suit -> (rank, suit)) suits) ranks)))

let shuffle deck =
  let new_deck = List.map (fun card -> (Random.bits (), card)) deck in
  let sort_deck = List.sort compare new_deck in
  List.map (fun weighted_card -> snd weighted_card) sort_deck

let points = function
  | Ace -> 11 
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




