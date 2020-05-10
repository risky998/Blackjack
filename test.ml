open OUnit2
open Deck
open Player
open Command
open State

(** TEST PLAN
    OUnit vs. Manual testing: We used OUnit tests mainly for the static
    modules of the game, such as Deck and Player. Manual testing by running 
    `make play` is used for dynamic components, such as State and Command.
    This is because playing the actual game of Blackjack involves many 
    different scenarios that a test suite may not be able to cover
    holistically. Hence, most of our testing of the functionality of the
    game was in manual testing of the game.

    The modules that are covered in the test suite include
    Deck (testing drawing cards, creating a deck, general characteristics
    of a card), Player (player characteristics and player win/lose status), 
    Command (parsing different commands), State (get functions).
    Our test cases are mainly glass box testing since we know the program
    structure and wrote the code ourselves. We also used our internal knowledge 
    of the code we wrote to guide the selection of the test data 
    and to improve the test design.

    The testing approach works for this system because this is mostly a
    dynamic game, where the player and AI make dynamic choices, thus
    manual testing is effective in this area. For more static components,
    we do use glass-box testing to help reveal any errors in the framework
    of our game implementation. *)

(** [cmp_set_like_lists lst1 lst2] compares two lists to see whether
    they are equivalent set-like lists.  That means checking two things.
    First, they must both be {i set-like}, meaning that they do not
    contain any duplicates.  Second, they must contain the same elements,
    though not necessarily in the same order. *)
let cmp_set_like_lists lst1 lst2 =
  let uniq1 = List.sort_uniq compare lst1 in
  let uniq2 = List.sort_uniq compare lst2 in
  List.length lst1 = List.length uniq1
  &&
  List.length lst2 = List.length uniq2
  &&
  uniq1 = uniq2

(** [pp_string s] pretty-prints string [s]. *)
let pp_string s = "\"" ^ s ^ "\""

(** [pp_list pp_elt lst] pretty-prints list [lst], using [pp_elt]
    to pretty-print each element of [lst]. *)
let pp_list pp_elt lst =
  let pp_elts lst =
    let rec loop n acc = function
      | [] -> acc
      | [h] -> acc ^ pp_elt h
      | h1::(h2::t as t') ->
        if n=100 then acc ^ "..."  (* stop printing long list *)
        else loop (n+1) (acc ^ (pp_elt h1) ^ "; ") t'
    in loop 0 "" lst
  in "[" ^ pp_elts lst ^ "]"

(* These tests demonstrate how to use [cmp_set_like_lists] and 
   [pp_list] to get helpful output from OUnit. *)
let cmp_demo = 
  [
    "order is irrelevant" >:: (fun _ -> 
        assert_equal ~cmp:cmp_set_like_lists ~printer:(pp_list pp_string)
          ["foo"; "bar"] ["bar"; "foo"]);
    (* Uncomment this test to see what happens when a test case fails.
       "duplicates not allowed" >:: (fun _ -> 
        assert_equal ~cmp:cmp_set_like_lists ~printer:(pp_list pp_string)
          ["foo"; "foo"] ["foo"]);
    *)
  ]

(********************************************************************
   End helper functions.
 ********************************************************************)

(* You are welcome to add strings containing JSON here, and use them as the
   basis for unit tests.  Or you can add .json files in this directory and
   use them, too.  Any .json files in this directory will be included
   by [make zip] as part of your CMS submission. *)

let deck_tests =
  [
    "testing rank" >:: (fun _ -> assert_equal Two (rank (Two, Clubs)));

    "testing rank with reduced ace" >:: (fun _ -> assert_equal (Ace 1) 
                                            (rank (Ace 1, Clubs)));

    "testing suit" >:: (fun _ -> assert_equal Clubs (suit (Two, Clubs)));

    "testing full_deck" >:: (fun _ -> assert_equal 52 
                                (Deck.size (full_deck ())));

    "testing shuffle" >:: (fun _ -> assert_equal 52 
                              (Deck.size (shuffle (full_deck ()))));

    "testing points" >:: (fun _ -> assert_equal 2 (points (Two, Clubs)));

    "testing points with regular Ace" >:: (fun _ -> assert_equal 11 
                                              (points (Ace 11, Clubs)));

    "testing points with reduced Ace" >:: (fun _ -> assert_equal 1 
                                              (points (Ace 1, Clubs)));

    "testing reduce_ace" >:: (fun _ -> assert_equal 1 
                                 ([(Ace 11, Spades); (Three, Clubs)] |> 
                                  reduce_ace |> List.hd |> points));

    "testing reduce_ace with two Aces" >:: (fun _ -> 
        assert_equal 11 
          ([(Ace 11, Spades); (Ace 11, Clubs)] |> 
           reduce_ace |> List.tl |> List.hd |> points));

    "testing string_of_card" >:: (fun _ -> assert_equal "8♣" 
                                     (string_of_card (Eight, Clubs)));

    "testing string_of_card with Ace" >:: (fun _ -> assert_equal "A♣" 
                                              (string_of_card (Ace 11, Clubs)));

    "testing can_split_pair" >:: (fun _ -> assert_equal true 
                                     (can_split_pair
                                        [(Three, Clubs); (Three, Spades)]));

    "testing can_split_pair2" >:: (fun _ -> assert_equal false 
                                      (can_split_pair
                                         [(Three, Clubs); (Four, Spades)]));
  ]

let j = Yojson.Basic.from_file "test.json"
let st = init_player j

let player_tests =
  [
    (* "testing get_value_hand" >:: (fun _ -> assert_equal 11 (get_value_hand [(Ace 11, Spades); (Two, Clubs); (Three, Diamonds)] 0)); *)

    "testing player_hand" >:: (fun _ -> assert_equal [] (player_hand st));

    "testing total_money" >:: (fun _ -> assert_equal 10 (total_money st));

    "testing value_hand" >:: (fun _ -> assert_equal 0 (value_hand st));

    "testing get_id" >:: (fun _ -> assert_equal "Player1" (get_id st));

    "testing is_ai" >:: (fun _ -> assert_equal false (is_ai st));

    "testing is_split" >:: (fun _ -> assert_equal false (is_split st));

    "testing is_dealer" >:: (fun _ -> assert_equal false (is_dealer st));

    "testing player_bet" >:: (fun _ -> 
        assert_equal 3 (st |> player_bet 7|> total_money));

    "testing get bet" >:: (fun _ -> 
        assert_equal 7 (st |> player_bet 7|> get_bet));

    "testing player_win" >:: (fun _ -> 
        assert_equal 14 (st |> player_bet 8 |> player_win|> total_money));

    "testing player_lose" >:: (fun _ -> 
        assert_equal 3 (st |> player_bet 7 |> player_lose|> total_money));

    "testing player_tie" >:: (fun _ -> 
        assert_equal 10 (st |> player_bet 7 |> player_tie|> total_money));

    "testing player_blackjack" >:: (fun _ -> 
        assert_equal 17 (st |> player_bet 7 |> player_blackjack|> total_money));

    "testing draw_card" >:: 
    (fun _ -> assert_equal 2 (st |> draw_card (Two, Clubs) |> value_hand));

    "testing draw_card with Ace" >:: 
    (fun _ -> assert_equal 11 (st |> draw_card (Ace 11, Clubs) |> value_hand));
  ]

let command_tests =
  [
    "testing Quit" >:: (fun _ -> assert_equal Quit (parse "quit"));

    "testing Hit" >:: (fun _ -> assert_equal Hit (parse "hit"));

    "testing Stay" >:: (fun _ -> assert_equal Stay (parse "stay"));

    "testing Double" >:: (fun _ -> assert_equal Double (parse "double"));

    "testing Split" >:: (fun _ -> assert_equal Split (parse "split"));  

    "testing Help" >:: (fun _ -> assert_equal Help (parse "help"));

    "testing Bet" >:: (fun _ -> assert_equal (Bet 10) (parse "bet 10"));

    "testing Empty" >:: (fun _ -> assert_raises Empty (fun () -> parse ""));

    "testing Malformed" >:: (fun _ -> 
        assert_raises Malformed (fun () -> parse "betting 10"));

    "testing remove_spaces" >:: (fun _ -> assert_equal Split (parse "  split")); 

    "testing Malformed 2" >:: (fun _ -> 
        assert_raises Malformed (fun () -> parse "s plit"));
  ]


let j2 = Yojson.Basic.from_file "init1.json"
let g = init_state j2
let state_tests = 
  [
    "testing get_players" >:: (fun _ -> 
        assert_equal 2 (g |> get_players |> List.length));

    "testing get_player" >:: (fun _ -> 
        assert_equal false (g |> get_player |> is_ai));

    "testing get_other_players" >:: (fun _ ->
        assert_equal 0 (g |> get_other_players |> List.length));

    "testing get_non_dealers" >:: (fun _ -> 
        assert_equal 1 (g |> get_non_dealers |> List.length));

    "testing get_dealer" >:: (fun _ -> 
        assert_equal true (g |> get_dealer |> is_dealer));

    "testing get_dealer_hand_value" >:: (fun _ -> 
        assert_equal 0 (g |> get_players |> get_dealer_hand_value));

    "testing stay" >:: (fun _ -> 
        assert_equal 1 (g |> stay (get_player g)|> stayed_length));

    "testing in_stayed" >:: (fun _ ->
        assert_equal true (g |> stay (get_player g)|> in_stayed (get_player g)));
  ]

let suite =
  "test suite for Blacjack"  >::: List.flatten [
    deck_tests;
    player_tests;
    command_tests;
    state_tests;
  ]

let _ = run_test_tt_main suite
