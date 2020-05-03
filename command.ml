(* Note: You may introduce new code anywhere in this file. *) 

type money = int

type command = 
  | Quit 
  | Bet of money
  | Stay
  | Double
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
    else String.lowercase_ascii h::remove_spaces t

(* [assert_int_of_string str] is true if int_on_string can be 
   called on x, false otherwise.  *)
let assert_int_of_string str = 
  try
    Some (int_of_string str) 
  with Failure _ -> None

let parse str =
  let str_list = String.split_on_char ' ' str |> remove_spaces in 
  match str_list with
  | [] -> raise Empty 
  | [s] -> if s = "quit" then Quit
    else if s = "hit" then Hit 
    else if s = "stay" then Stay
    else if s = "money" then Money 
    else if s = "help" then Help 
    else if s = "double" then Double
    else raise Malformed
  | s::m::[] -> if s = "bet" then 
      begin
        match assert_int_of_string m with
        | Some money -> Bet money
        | None -> raise Malformed
      end
    else (raise Malformed)
  | _ -> (raise Malformed)
