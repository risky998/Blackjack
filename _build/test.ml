open OUnit2
open Deck
open State

(********************************************************************
   Here are some helper functions for your testing of set-like lists. 
 ********************************************************************)

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

    "testing suit" >:: (fun _ -> assert_equal Clubs (suit (Two, Clubs)));

    "testing full_deck" >:: (fun _ -> assert_equal 52 (Deck.size (full_deck ())));

    "testing shuffle" >:: (fun _ -> assert_equal 52 (Deck. size (shuffle (full_deck ()))));

    "testing points" >:: (fun _ -> assert_equal 2 (points (Two, Clubs)));

    "testing draw_start" >:: (fun _ -> assert_equal ([(Ace 11, Spades); (Two, Clubs)], [(Three, Diamonds)])
                                 (draw_start [(Two, Clubs); (Four, Hearts); (Six, Hearts)]));

    "testing reduce_ace1" >:: (fun _ -> assert_equal 1 ([(Ace 11, Spades); (Ace 11, Clubs)] |> reduce_ace |> List.hd |> points));

    "testing reduce_ace2" >:: (fun _ -> assert_equal 11 ([(Ace 11, Spades); (Ace 11, Clubs)] |> reduce_ace |> List.tl |> List.hd |> points));

    (* "testing draw_start" >:: (fun _ -> assert_equal ([(Ace 11, Spades); (Two, Clubs)], [(Three, Diamonds)]) 
                                 [(Ace 11, Spades); (Two, Clubs); (Three, Diamonds)] |> draw_start); *)

    (* "testing draw" >:: (fun _ -> assert_equal ([(Ace 11, Spades)], [(Two, Clubs); (Three, Diamonds)]) 
                           (draw [(Ace 11, Spades); (Two, Clubs), (Three, Diamonds)]));    *)
  ]

let command_tests =
  [
    (* TODO: add tests for the Command module here *)
  ]

let state_tests =
  [
    (* TODO: add tests for the State module here *)
  ]

let suite =
  "test suite for Blacjack"  >::: List.flatten [
    deck_tests;
    command_tests;
    state_tests;
  ]

let _ = run_test_tt_main suite