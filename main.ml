open Deck
open Player
open State
open Command
open Ai

(** [print_cards] is a string of the elements of a string list. *)
let print_cards = function
  | [] -> ""
  | [s] -> s
  | h::t -> List.fold_left (fun acc e -> acc^", "^e) h t

let ai_interface ai game = 
  let ai_bet = get_bet ai in
  if ai_bet = 0 then 
    match bet 20 ai game with
    | Legal new_game -> 
      ANSITerminal.(print_string [yellow] ("\nPlayer "^(get_id ai)^"bets"^(string_of_int 20)^"\n"));
      new_game
    | Illegal -> ANSITerminal.(print_string [yellow] 
                                 "\nError: Not enough money!\n");
      game
  else let top_card_val = State.top_card_value game in
    match hit_stay_strategy top_card_val ai with
    | Stay -> 
      ANSITerminal.(print_string [yellow] ("\nPlayer "^(get_id ai)^"stays\n"));
      game
    | Hit -> 
      begin 
        match hit ai game with
        | Legal new_game -> 
          ANSITerminal.(print_string [yellow] ("\nPlayer "^(get_id ai)^"hits\n"));          new_game
        | Illegal -> ANSITerminal.(print_string [yellow] 
                                     "\nError: No cards available!\n");
          game
      end
    | _ -> game

let dealer_interface dealer game = 
  match dealer_strategy dealer with
  | Stay -> game
  | Hit -> 
    begin 
      match hit dealer game with
      | Legal new_game -> new_game
      | Illegal -> ANSITerminal.(print_string [yellow] 
                                   "\nError: No cards available!\n");
        game
    end
  | _ -> game

(** [game_interface st] is the game interface that prompts the user
    to enter a command and updates the state [st] accordingly. *)
let rec game_interface game : State.t= 
  let main_player = get_player game in
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
  let dealer_len = snd (dealer_info game) in
  let dealer_top_card = fst (dealer_info game) in
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
    (* | exception End_of_file -> ()      *)
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
              new_game
            | Illegal -> ANSITerminal.(print_string [yellow] 
                                         "\nError: Not enough money!\n");
              game_interface game
          end
        | _ -> 
          ANSITerminal.(print_string [yellow] "\nYou need to bet first!\n");
          game_interface game
      end
  else 
    match read_line () with
    (* | exception End_of_file -> ()      *)
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
            | Legal new_game -> new_game
            | Illegal -> ANSITerminal.(print_string [yellow] 
                                         "\nError: No cards available!\n");
              game_interface game                        
          end
        | _ -> 
          ANSITerminal.(print_string [yellow] "\nError: Invalid command!\n");
          game_interface game
      end

(** [loop_through_players players] is a game state of blackjack after going 
    through each player in [players] and 
    giving everyone a turn. *)
(* let rec loop_through_players game = function
   | [] -> game
   | p::ps when Player.is_cpu p -> 
    let () = ANSITerminal.(print_string [blue] (
        "-------------------------------------------------" ^
        "\n\nIt is " ^ Player.get_name p ^ "'s turn now.\n")) in
    loop_through_players (play_cpu game (Player.get_id p)) ps
   | p::ps -> 
    if Player.is_dealer p then
      let () = ANSITerminal.(print_string [blue] (dealers_turn))  in
      play_dealer game p
    else 
      let () = ANSITerminal.(print_string [blue] (
          "-------------------------------------------------" ^
          "\n\n" ^  Player.get_name p ^ start_player_turn));
        print_string "> ";
      in
      match read_line () with
      |"quit" -> print_endline quit; exit 0
      |_ -> loop_through_players (play_turn game (Player.get_id p)) ps *)

let rec turn game players =
  let rec turn_helper game players = 
    match players with 
    | [] -> game
    | h::t when is_ai h -> turn_helper (ai_interface h game) t;
    | h::t when is_dealer h -> turn_helper (dealer_interface h game) t;
    | h::t -> turn_helper (game_interface game) t;
  in turn_helper game players

(** [play_game f] starts the adventure in file [f]. *)
let play_game f =
  let game = init_state (Yojson.Basic.from_file f) in
  turn game (get_players game)

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
