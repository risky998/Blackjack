open Deck
open Player
open State
open Command

(** [print_cards] is a string of the elements of a string list. *)
let print_cards = function
  | [] -> ""
  | [s] -> s
  | h::t -> List.fold_left (fun acc e -> acc^", "^e) h t

(** [game_interface st] is the game interface that prompts the user
    to enter a command and updates the state [st] accordingly. *)
let rec game_interface game = 
  let main_player = get_player game in
  (* if ai_turn then
     execute ai bet/hit
     Ansitterminal
     change turn
     game_interfce game *)
  let hand = List.map(fun card -> string_of_card card) (player_hand main_player) in
  ANSITerminal.
    (print_string [blue] ("Cards: ["^
                          print_cards (hand)^"]\n"));
  let player_value = value_hand main_player in
  ANSITerminal.
    (print_string [blue] ("Hand value: "^
                          (string_of_int player_value)^"\n"));
  let player_money = total_money main_player in
  ANSITerminal.
    (print_string [blue] ("Money: "^
                          (string_of_int player_money)^"\n"));
  let dealer = get_dealer game in
  let dealer_hand = player_hand dealer in
  let dealer_len = List.length dealer_hand in
  let dealer_top_card = if dealer_len = 0 then "" else string_of_card (List.nth dealer_hand (dealer_len-1)) in
  ANSITerminal.
    (print_string [blue] ("Dealer's Top Card: "^
                          (dealer_top_card)^"; Dealer's Cards: "^(string_of_int dealer_len)^"\n"));
  let other_players = get_other_players game in
  List.iter(fun p -> 
      let other_player_hand = List.map(fun card -> string_of_card card) (player_hand p) in
      ANSITerminal.
        (print_string [blue] ("Player "^(get_id p)^"'s hand: "^
                              (print_cards other_player_hand)^"\n"));) other_players;
  let player_bet = get_bet main_player in
  if player_bet = 0 then
    match read_line () with
    | exception End_of_file -> ()     
    | command -> 
      begin 
        match (parse command) with
        | exception Empty -> 
          ANSITerminal.(print_string [yellow] "\nYou need to bet first!\n");
          game_interface game
        | exception Malformed -> 
          ANSITerminal.(print_string [yellow] "\nYou need to bet first!\n");
          game_interface game
        | Quit -> 
          ANSITerminal.(print_string [blue] "\nThanks for playing!\n");
          exit 0
        | Bet money -> 
          begin 
            match bet money main_player game with
            | Legal new_game -> 
              game_interface new_game
            | Illegal -> ANSITerminal.(print_string [yellow] 
                                         "\nError: Not enough money!\n");
          end
        | _ -> 
          ANSITerminal.(print_string [yellow] "\nYou need to bet first!\n");
          game_interface game
      end
  else 
    match read_line () with
    | exception End_of_file -> ()     
    | command -> 
      begin 
        match (parse command) with
        | exception Empty -> 
          ANSITerminal.(print_string [yellow] 
                          "\nError: Please enter a command!\n");
          game_interface game
        | exception Malformed -> 
          ANSITerminal.(print_string [yellow] "\nError: Invalid command!\n");
          game_interface game
        | Quit -> 
          ANSITerminal.(print_string [blue] "\nThanks for playing!\n");
          exit 0
        | Hit -> 
          begin 
            match hit main_player game with
            | Legal new_game -> game_interface new_game
            | Illegal -> ANSITerminal.(print_string [yellow] 
                                         "\nError: No cards available!\n");
          end
        | _ -> 
          ANSITerminal.(print_string [yellow] "\nError: Invalid command!\n");
          game_interface game
      end

(** [play_game f] starts the adventure in file [f]. *)
let play_game f =
  let game = init_state (Yojson.Basic.from_file f) in
  game_interface game

(** [main ()] prompts for the game to play, then starts it. *)
let main () =
  ANSITerminal.(print_string [red]
                  "\n\nWelcome to the 3110 Blackjack Game engine.\n");
  print_endline "Please enter the number of AI CPU's you wish to play against (0, 1 or).\n";
  print_string  "> ";
  match read_line () with
  | exception End_of_file -> ()
  | file_name -> play_game file_name

(* Execute the game engine. *)
let () = main ()
