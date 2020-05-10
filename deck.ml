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

let empty = []

let size (deck:t) : int = List.length deck

let full_deck (unit:unit) : t = ((List.concat (List.map (fun rank -> 
    List.map (fun suit -> (rank, suit)) suits) ranks)))

let shuffle deck =
  QCheck.Gen.(generate1 (shuffle_l deck))

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

let rec reduce_ace hand = 
  match hand with
  | [] -> []
  | (Ace 11, suit)::t -> (Ace 1,suit)::t
  | h::t -> h::reduce_ace t

let draw_start deck = 
  match deck with
  | c1::c2::t -> ([c1; c2], shuffle t)
  | d -> ([], d)

let draw deck = 
  match deck with
  | [] -> None
  | h::t -> Some (h, t)

let string_of_card (card:card) : string = 
  let r = 
    match fst card with
    | Ace x -> "A"
    | Two -> "2"
    | Three -> "3"
    | Four -> "4"
    | Five -> "5"
    | Six -> "6"
    | Seven -> "7" 
    | Eight -> "8"
    | Nine -> "9"
    | Ten -> "10" 
    | Jack -> "J" 
    | Queen -> "Q"
    | King -> "K"
  in 
  let s = 
    match snd card with
    | Clubs -> "♣"
    | Diamonds -> "♦"
    | Hearts -> "♥"
    | Spades -> "♠" in
  r ^ s

let can_split_pair hand =
  if List.length hand = 2 then
    let first_card = List.nth hand 0 in
    let second_card = List.nth hand 1 in
    match first_card, second_card with
    | (Ace x, _), (Ace y, _) -> true
    | (r1, _), (r2, _) -> r1 = r2
  else false