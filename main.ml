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

(** [ai_bet_interface ai game] updates the state [game] accordingly after 
    the AI bets. *)
let ai_bet_interface ai game = 
  let ai_money = total_money ai in
  let player_bet = get_bet (get_player game) in
  let bet_amount =
    begin
      match ai_money, player_bet with
      | x,y when y>=x -> x
      | x,y when y = 0 -> min x 50
      | _ -> player_bet
    end in
  match bet bet_amount ai game with
  | Legal new_game -> 
    ANSITerminal.(print_string [yellow] ("\nPlayer "^(get_id ai)^
                                         " bets "^
                                         (string_of_int bet_amount)^"\n"));
    new_game
  | Illegal -> game

(** [ai_play_interface ai game] updates the state [game] accordingly after 
    the AI hits, stays, or doubles. *)
let ai_play_interface ai game = 
  let dealer_top_card = 
    begin
      match dealer_top_card game with
      | None -> 0
      | Some c -> points c
    end
  in
  match hit_double_stay_strategy dealer_top_card ai with
  | Stay -> 
    ANSITerminal.(print_string [yellow] ("\nPlayer "^(get_id ai)^
                                         " stays\n"));
    stay ai game
  | Hit -> 
    begin 
      match hit ai game with
      | Legal new_game -> 
        ANSITerminal.(print_string [yellow] ("\nPlayer "^
                                             (get_id ai)^" hits\n"));          
        new_game
      | Illegal -> game
    end
  | Double -> 
    begin 
      match double ai game with
      | Legal new_game -> 
        ANSITerminal.(print_string [yellow] ("\nPlayer "^
                                             (get_id ai)^" doubles down.\n"));          
        new_game
      | Illegal -> game
    end
  | _ -> game

(** [ai_interface ai game] updates the state [game] accordingly after 
    the AI executes its commands. *)
let ai_interface ai game = 
  if get_bet ai = 0 then 
    ai_bet_interface ai game
  else 
    ai_play_interface ai game

(** [dealer_interface game] updates the state [game] accordingly after 
    the dealer executes its commands. *)
let rec dealer_interface game = 
  let dealer = get_dealer game in
  let dealer_hand = List.map(fun card -> string_of_card card) 
      (player_hand dealer) in
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

(** [player_interface_info player game] displays information on the terminal
    during the player's turn, such as the player's hand, money, hand value,
    cards of other players, and the dealer's top card. *)
let player_interface_info player game = 
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
      let other_player_hand = List.map(fun card -> string_of_card card) 
          (player_hand p) in
      ANSITerminal.
        (print_string [blue] ("Player "^(get_id p)^"'s hand: "^
                              (print_cards other_player_hand)^"\n"));) 
    other_players

(** [player_interface_help] is a string detailing instructions to help
    players with the terminal commands to play blackjack *)
let player_interface_help = 
  "\nPlayer Help Menu:
Type \"bet x\" to bet an x amount of money.
Type \"hit\" to receive another card.
Type \"stay\" if you are satisfied with your hand.
Type \"double\" to double your initial bet in return for a single card.
Type \"split\" to split your hand if your starting cards have the sa
Type \"quit\" to leave the game.
\n"

(** [player_bet_interface player game] updates the state [game] accordingly 
    after the player bets. *)
let rec player_bet_interface player game = 
  match read_line () with
  | command -> 
    begin 
      match (parse command) with
      | exception Empty -> 
        ANSITerminal.(print_string [red] "\nYou need to bet first!\n");
        player_bet_interface player game
      | exception Malformed -> 
        ANSITerminal.(print_string [red] "\nYou need to bet first!\n");
        player_bet_interface player game
      | Quit -> 
        ANSITerminal.(print_string [blue] "\nThanks for playing!\n");
        exit 0
      | Bet money -> 
        begin 
          match bet money player game with
          | Legal new_game -> 
            new_game
          | Illegal -> if money = 0 then 
              ANSITerminal.(print_string [red] 
                              "\nError: You need to bet something!\n")
            else 
              ANSITerminal.(print_string [red] 
                              "\nError: Not enough money!\n");
            player_bet_interface player game
        end
      | Help -> 
        ANSITerminal.(print_string [white] player_interface_help);
        player_interface_info player game;
        player_bet_interface player game
      | _ -> 
        ANSITerminal.(print_string [red] "\nYou need to bet first!\n");
        player_bet_interface player game
    end

(** [player_interface player game] is the game interface that prompts the user
    to enter a command and updates the state [game] accordingly. *)
let rec player_interface player game = 
  player_interface_info player game;
  let player_bet = get_bet player in
  if player_bet = 0 then
    player_bet_interface player game
  else 
    match read_line () with
    | command -> 
      begin 
        match (parse command) with
        | exception Empty -> 
          ANSITerminal.(print_string [red] 
                          "\nError: Please enter a command!\n");
          player_interface player game
        | exception Malformed -> 
          ANSITerminal.(print_string [red] "\nError: Invalid command!\n");
          player_interface player game
        | Quit -> 
          ANSITerminal.(print_string [blue] "\nThanks for playing!\n");
          exit 0
        | Stay -> stay player game
        | Hit -> 
          begin 
            match hit player game with
            | Legal new_game -> 
              new_game
            | Illegal -> ANSITerminal.(print_string [red] 
                                         "\nError: No cards available!\n");
              player_interface player game                        
          end
        | Double -> 
          begin 
            match double player game with
            | Legal new_game -> 
              new_game
            | Illegal -> ANSITerminal.(print_string [red] 
                                         "\nError: Double Failed!\n");
              player_interface player game                        
          end
        | Split -> 
          begin
            match split player game with
            | Legal new_game -> 
              new_game
            | Illegal -> ANSITerminal.(print_string [red] 
                                         ( "\nError: You can only split with"^
                                           " a pair!\n"));
              player_interface player game      
          end
        | Help -> 
          ANSITerminal.(print_string [white] player_interface_help);
          player_interface player game
        | _ -> 
          ANSITerminal.(print_string [red] "\nError: Invalid command!\n");
          player_interface player game
      end

(** [make_json] players creates a valid JSON representation based on the
    Player list [players]. *)
let make_json players = 
  let p_ids = List.map Player.get_id players in 
  let p_money = List.map Player.total_money players in
  let p_dealer = List.map Player.is_dealer players in 
  let p_ai = List.map Player.is_ai players in
  let rec json_helper id total_money dealer ai = 
    match id, total_money, dealer, ai with
    | [], [], [], [] -> []
    | i::ri, m::rm, d::rd, a::ra ->
      let player = `Assoc [("id", `String i);
                           ("total_money",`Int m);
                           ("dealer", `Bool d);
                           ("ai", `Bool a);] in 
      player::json_helper ri rm rd ra
    |_ -> raise (Failure "Corrupted game") in
  let players_json = json_helper p_ids p_money p_dealer p_ai in 
  `Assoc [("players", `List players_json)]

(** [save_game game] prompts the player to enter a file name and saves
    the state [game] to that file with a json extension. *)
let rec save_game game =
  print_string  ("\n\nEnter game name: "); 
  let format_file_name = String.trim (read_line ()) |> 
                         String.map (fun c -> if c = ' ' then '_' else c) |> 
                         String.lowercase_ascii in
  match format_file_name with
  | "" -> print_endline "Enter a valid file name!";
    save_game game
  | s -> 
    print_endline ("Game saved as "^s^".json");
    Yojson.to_file (s^".json") (make_json (get_players game));
    exit 0 

(** [save_game_message game] prompts the player to save a game or
    quit the game. *)
let rec save_game_message game = 
  print_endline  ("\n\nWould you like to save this game? (y/n)"); 
  print_string  "> "; 
  begin 
    match read_line () with
    | "yes" | "y" -> 
      save_game game
    | "no" | "n" -> exit 0
    | _ -> print_endline "Enter a valid command!";
  end

(** [turn game players] runs the game by first allowing all the non-dealer
    players to execute commands, then letting the dealer execute its command.
    After all players have executed their commands, the state [game] is updated
    accordingly with the money lost/won by each player and a new game is 
    started with everyone's hands reset.  *)
let rec turn game players =
  if stayed_length game < List.length (get_non_dealers game) then
    match players with
    | [] -> 
      turn game (get_non_dealers game)
    | h::t when is_ai h -> 
      if (in_stayed h game) then turn game t 
      else 
        ANSITerminal.(print_string [blue]   
                        ("___________________________________________________"^
                         "\n\nIt is Player " ^(Player.get_id h)^
                         "'s turn now.\n"));
      turn (ai_interface h game) t
    | h::t -> 
      if (in_stayed h game) then turn game t 
      else 
        ANSITerminal.(print_string [blue]   
                        ("___________________________________________________"^
                         "\n\nIt is your turn now.\n"));
      turn (player_interface h game) t
  else 
    ANSITerminal.(print_string [blue]   
                    ("___________________________________________________"^
                     "\n\nIt is dealer's turn now.\n"));
  let end_game = dealer_interface game in
  let end_players = get_non_dealers end_game in
  let dealer_value = Player.value_hand (State.get_dealer end_game) in
  ANSITerminal.(print_string [blue] ("\nDealer's hand: "^
                                     string_of_int dealer_value));
  List.iter (fun p -> match game_end_status dealer_value p with
      | PlayerLose -> ANSITerminal.(print_string [blue] 
                                      ("\nPlayer "^(get_id p)^" lost "^
                                       string_of_int (get_bet p)^
                                       "; Hand Value: "^
                                       string_of_int (value_hand p)))
      | PlayerWin -> 
        let money_won = int_of_float (float_of_int (get_bet p) *. 1.5) in
        ANSITerminal.(print_string [blue] 
                        ("\nPlayer "^(get_id p)^" won "^
                         string_of_int (money_won)^
                         "; Hand Value: "^
                         string_of_int (value_hand p)))
      | PlayerBlackJack -> ANSITerminal.(print_string [blue] 
                                           ("\nPlayer "^(get_id p)^" won "^
                                            string_of_int ((get_bet p)*2)^
                                            "; Hand Value: "^
                                            string_of_int (value_hand p)))
      | PlayerTie -> ANSITerminal.(print_string [blue] 
                                     ("\nPlayer "^(get_id p)^
                                      " tied"^
                                      "; Hand Value: "^
                                      string_of_int (value_hand p)))
    ) end_players;
  let new_game = reset end_game in
  if (total_money (get_player new_game)) <= 0 then 
    begin
      ANSITerminal.(print_string [blue] 
                      ("\n\nYou have run out of money :("^
                       "\nThanks for playing!\n"));
      exit 0
    end
  else new_game_message new_game

(** [new_game_message game] prompts the player to start a new game or 
    quit and/or save the game. *)
and new_game_message game = 
  if List.length (get_non_dealers game) = 0 then () else
    print_endline  ("\n\nPlay again? (y/n)"); 
  print_string  "> "; 
  match read_line () with
  | "yes" | "y" -> ANSITerminal.(print_string [red]   
                                   "\n\nNEW GAME\n");
    turn (game) (get_non_dealers game)
  | "no" | "n" -> save_game_message game
  | _ -> print_endline "Enter a valid command!";
    new_game_message game

(** [play_game f] starts the adventure in file [f]. *)
let play_game game =
  turn game (get_non_dealers game)

(** [main ()] prompts for the game to play, then starts it. *)
let main () =
  ANSITerminal.(print_string [blue]
                  "\n\nWelcome to the 3110 Blackjack Game engine.\n");
  print_endline ("Please enter the name of the game file you wish to load.\n");
  print_string  "> ";
  match read_line () with
  | exception End_of_file -> ()
  | file_name -> 
    let game = init_state (Yojson.Basic.from_file file_name) in 
    play_game game

(* Execute the game engine. *)
let () = main ()