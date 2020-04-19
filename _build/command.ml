(* Note: You may introduce new code anywhere in this file. *) 

type money = int

type command = 
  | Quit 
  | Bet of money
  | Stay
  | Hit
  | Money
  | Help

exception Empty

exception Malformed

(** [remove_spaces lst] is a string list without all elements that are "". 
    If [lst] is empty, [remove_spaces] returns []. *)
let rec remove_spaces = function
  | [] -> []
  | h::t -> 
    if h = "" then remove_spaces t
    else h::remove_spaces t

let parse str =
  let str_list = String.split_on_char ' ' str |> remove_spaces in 
  match str_list with
  | [] -> raise Empty 
  | [s] -> if s = "quit" then Quit
    else if s = "hit" then Hit 
    else if s = "stay" then Stay
    else if s = "money" then Money 
    else if s = "help" then Help 
    else raise Malformed
  | s::m::[] -> if s = "bet" then (Bet (int_of_string m)) 
    else (raise Malformed)
  | _ -> (raise Malformed)
