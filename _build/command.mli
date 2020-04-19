(**
   Parsing of blackjack commands.
*)

(** The type [money] represents the amount of money that a player can bet 
    on a hand.
    Each element of the list represents a word of the object 
    phrase, where a {i word} is defined as a consecutive sequence of non-space 
    characters.  Thus, no element of the list should contain any leading,
    internal, or trailing spaces.  The list is in the same order as the words 
    in the original player command.  For example:
    - If the player command is ["go clock tower"], then the object phrase is 
      [["clock"; "tower"]].
    - If the player command is ["go clock     tower"], then the object phrase is
      again [["clock"; "tower"]]. 

    [money] is the amount of money the player wants to bet *)
type money = int

(** The type [command] represents a player command that is decomposed
    into a verb and possibly an object phrase. *)
type command = 
  | Quit 
  | Bet of money
  | Stay
  | Hit
  | Money
  | Help

(** Raised when an empty command is parsed. *)
exception Empty

(** Raised when a malformed command is encountered. *)
exception Malformed


(** [parse str] parses a player's input into a [command], as follows. The first
    - [parse "quit"] is [Quit]. 

    Requires: [str] contains only numbers 0-9) and space 
    characters (only ASCII character code 32; not tabs or newlines, etc.).

    Raises: [Empty] if [str] is the empty string or contains only spaces. 

    Raises: [Malformed] if the command is malformed. A command
    is malformed if the verb is not a valid type command.
    or if the verb is "quit"/"hit"/"stay"/"money"/"help" and 
    there is a non-empty object phrase,
    or if the verb is "bet" and there is an empty object phrase.*)
val parse : string -> command