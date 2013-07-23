-module(cards).
-export([make_deck/0]).

make_deck() -> 
  [{X, Y} || X <- ["A", 2, 3, 4, 5, 6, 7, 8, 9, 10, "J", "Q", "K"], Y <- ["Clubs", "Diamonds", "Hearts", "Spades"]].
