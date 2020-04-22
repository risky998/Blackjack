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
  (* failwith "No" *)
  let main_player = get_player game in
  let player_hand = List.map(fun card -> string_of_card card) (player_hand main_player) in
  ANSITerminal.
    (print_string [red] ("Cards: ["^
                         print_cards (player_hand)^"]\n"));
  match read_line () with
  | exception End_of_file -> ()     
  | command -> begin match (parse command) with
      | Quit -> 
        ANSITerminal.(print_string [blue] "\nThanks for playing!\n");
        exit 0
      | _ -> 
        ANSITerminal.(print_string [blue] "\nThanks for playing!\n");
        exit 0
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
