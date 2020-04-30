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
      ANSITerminal.(print_string [yellow] ("\nPlayer "^(get_id ai)^" bets "^(string_of_int 20)^"\n"));
      new_game
    | Illegal -> game
  else 
    let dealer_top_card = 
      begin
        match dealer_top_card game with
        | None -> 0
        | Some c -> points c
      end
    in
    match hit_stay_strategy dealer_top_card ai with
    | Stay -> 
      ANSITerminal.(print_string [yellow] ("\nPlayer "^(get_id ai)^" stays\n"));
      stay ai game
    | Hit -> 
      begin 
        match hit ai game with
        | Legal new_game -> 
          ANSITerminal.(print_string [yellow] ("\nPlayer "^(get_id ai)^" hits\n"));          
          new_game
        | Illegal -> game
      end
    | _ -> game

let rec dealer_interface game = 
  let dealer = get_dealer game in
  let dealer_hand = List.map(fun card -> string_of_card card) (player_hand dealer) in
  ANSITerminal.
    (print_string [blue] ("Cards: ["^
                          print_cards (dealer_hand)^"]\n"));
  let player_value = value_hand dealer in
  ANSITerminal.
    (print_string [blue] ("Hand value: "^
                          (string_of_int player_value)^"\n"));
  match dealer_strategy dealer with
  | Stay ->         
    ANSITerminal.(print_string [yellow] ("\nDealer stays\n"));  
    game
  | Hit -> 
    begin 
      match hit dealer game with
      | Legal new_game -> 
        ANSITerminal.(print_string [yellow] ("\nDealer hits\n"));  
        dealer_interface new_game
      | Illegal -> dealer_interface game
    end
  | _ -> game

(** [game_interface st] is the game interface that prompts the user
    to enter a command and updates the state [st] accordingly. *)
let rec game_interface player game : State.t= 
  let hand = List.map(fun card -> string_of_card card) (player_hand player) in
  ANSITerminal.
    (print_string [blue] ("Cards: ["^
                          print_cards (hand)^"]\n"));
  let player_value = value_hand player in
  ANSITerminal.
    (print_string [blue] ("Hand value: "^
                          (string_of_int player_value)^"\n"));
  let player_money = total_money player in
  ANSITerminal.
    (print_string [blue] ("Money: "^
                          (string_of_int player_money)^"\n"));
  let dealer_top_card = 
    begin
      match dealer_top_card game with
      | None -> ""
      | Some c -> string_of_card c
    end
  in
  ANSITerminal.
    (print_string [blue] ("Dealer's Top Card: "^dealer_top_card^"\n"));
  let other_players = get_other_players game in
  List.iter(fun p -> 
      let other_player_hand = List.map(fun card -> string_of_card card) (player_hand p) in
      ANSITerminal.
        (print_string [blue] ("Player "^(get_id p)^"'s hand: "^
                              (print_cards other_player_hand)^"\n"));) other_players;
  let player_bet = get_bet player in
  if player_bet = 0 then
    match read_line () with
    | command -> 
      begin 
        match (parse command) with
        | exception Empty -> 
          ANSITerminal.(print_string [yellow] "\nYou need to bet first!\n");
          game_interface player game
        | exception Malformed -> 
          ANSITerminal.(print_string [yellow] "\nYou need to bet first!\n");
          game_interface player game
        | Quit -> 
          ANSITerminal.(print_string [blue] "\nThanks for playing!\n");
          exit 0
        | Bet money -> 
          begin 
            match bet money player game with
            | Legal new_game -> 
              new_game
            | Illegal -> if money = 0 then 
                ANSITerminal.(print_string [yellow] 
                                "\nError: You need to bet something!\n")
              else 
                ANSITerminal.(print_string [yellow] 
                                "\nError: Not enough money!\n");
              game_interface player game
          end
        | _ -> 
          ANSITerminal.(print_string [yellow] "\nYou need to bet first!\n");
          game_interface player game
      end
  else 
    match read_line () with
    | command -> 
      begin 
        match (parse command) with
        | exception Empty -> 
          ANSITerminal.(print_string [yellow] 
                          "\nError: Please enter a command!\n");
          game_interface player game
        | exception Malformed -> 
          ANSITerminal.(print_string [yellow] "\nError: Invalid command!\n");
          game_interface player game
        | Quit -> 
          ANSITerminal.(print_string [blue] "\nThanks for playing!\n");
          exit 0
        | Stay -> stay player game
        | Hit -> 
          begin 
            match hit player game with
            | Legal new_game -> 
              new_game
            | Illegal -> ANSITerminal.(print_string [yellow] 
                                         "\nError: No cards available!\n");
              game_interface player game                        
          end
        | _ -> 
          ANSITerminal.(print_string [yellow] "\nError: Invalid command!\n");
          game_interface player game
      end

let rec turn game players =
  if stayed_length game < 3 then
    match players with
    | [] -> 
      turn game (get_non_dealers game)
    | h::t when is_ai h -> 
      if (in_stayed h game) then turn game t 
      else 
        ANSITerminal.(print_string [blue]   
                        ("___________________________________________________"^
                         "\n\nIt is Player " ^(Player.get_id h)^ "'s turn now.\n"));
      turn (ai_interface h game) t
    | h::t -> 
      if (in_stayed h game) then turn game t 
      else 
        ANSITerminal.(print_string [blue]   
                        ("___________________________________________________" ^
                         "\n\nIt is your turn now.\n"));
      turn (game_interface h game) t
  else 
    ANSITerminal.(print_string [blue]   
                    ("___________________________________________________" ^
                     "\n\nIt is dealer's turn now.\n"));
  (* ANSITerminal.(print_string [yellow] (string_of_int (value_hand dealer))); *)
  let end_game = dealer_interface game in
  let end_players = get_non_dealers end_game in
  let dealer_value = Player.value_hand (State.get_dealer end_game) in
  ANSITerminal.(print_string [red] ("\nDealer's hand: "^string_of_int dealer_value));
  List.iter (fun p -> match game_end_status dealer_value p with
      | PlayerLose ->  ANSITerminal.(print_string [red] ("\nPlayer "^(get_id p)^" lost "^string_of_int (get_bet p)^" tempVal: "^string_of_int (value_hand p)))
      | PlayerWin ->   ANSITerminal.(print_string [red] ("\nPlayer "^(get_id p)^" won "^string_of_int ((get_bet p)*2)^" tempVal: "^string_of_int (value_hand p)))
      | PlayerBlackJack -> ANSITerminal.(print_string [red] ("\nPlayer "^(get_id p)^" won "^string_of_int ((get_bet p)*2)^" tempVal: "^string_of_int (value_hand p)))
      | PlayerTie -> ANSITerminal.(print_string [red] ("\nPlayer "^(get_id p)^" tied"^" tempVal: "^string_of_int (value_hand p)))
    ) end_players;
  let new_game = reset end_game in 
  ANSITerminal.(print_string [red]   
                  "\n\nNEW GAME\n");
  turn (new_game) (get_players (new_game))

(** [play_game f] starts the adventure in file [f]. *)
let play_game game =
  turn game (get_non_dealers game)

(** [main ()] prompts for the game to play, then starts it. *)
let main () =
  ANSITerminal.(print_string [red]
                  "\n\nWelcome to the 3110 Blackjack Game engine.\n");
  print_endline "Please enter the number of AI CPU's you wish to play against (0, 1 or).\n";
  print_string  "> ";
  match read_line () with
  | exception End_of_file -> ()
  | file_name -> 
    let game = init_state (Yojson.Basic.from_file file_name) in 
    play_game game

(* Execute the game engine. *)
let () = main ()
