# Blackjack

A playable blackjack game, built in OCaml.

## Instructions to install

Download the zip file containing this code repository from github.
Unzip the file and in the folder repository, run "make clean" and "make build".

## Starting a game

Run "make play". The game engine will prompt you to enter a .json game file.
We have included 3 default games:

- "init1.json" is a 1 player vs. dealer game
- "init2.json" is a 2 player vs. dealer game
- "init2.json" is a 3 player vs. dealer game

## How to play

An overview of Blackjack rules can be found [here](https://bicyclecards.com/how-to-play/blackjack/)
We have adapted most of the Blackjack rules into our game.

Here are the following player commands:

- "quit": Ends the game.
- "help": In-game instructions on how to play Blackjack.
- "bet <money>": MUST be played during the first round of the game and you can only bet a minimum amount strictly greater than 0 and a maximum amount of the total money you have.
- "hit": Draw a card from the deck.
- "stay": Ends your turn and you can no longer execute any commands. Once all the non-dealers have chosen to stay, the dealer will begin his turn.
- "double": Double the original bet you placed and draw a card. You can no longer choose to hit or stay.
- "split": If the player has a pair, splits the players hand into two separate hands. The player can then play these hands separately. At the end of the turn, the rewards are cumulated back together.

## Creating custom game files

A custom game file can be created but must always observe the following rules:

1. Only the fields "id", "total_money", "dealer", "ai" can exist
2. There must be one non-AI player who is not the dealer and one AI dealer

```json
{
  "players": [
    {
      "id": "1",
      "total_money": 300,
      "dealer": false,
      "ai": false
    },
    {
      "id": "2",
      "total_money": 300,
      "dealer": true,
      "ai": true
    }
  ]
}
```
