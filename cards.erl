-module(cards).
-export([make_deck/0]).
-export([shuffle/1]).

make_deck() -> 
  [{X, Y} || X <- ["A", 2, 3, 4, 5, 6, 7, 8, 9, 10, "J", "Q", "K"], Y <- ["Clubs", "Diamonds", "Hearts", "Spades"]].

% call the private shuffle/2, the 1st param is the remaining cards to shuffle, the 2nd param is the result
shuffle(List) -> shuffle(List, []).

% when the original list reaches empty, stop the recursion and return the result list
shuffle([], Acc) -> Acc;
shuffle(List, Acc) ->
  % split the List, 1st param is a random value between 0 ~ (List length - 1)
  % not use 1 ~ List length is to make sure the 2nd part of splitted list has at least 1 element, 
  % so that [H|T] won't create exception
  {Leading, [H | T]} = lists:split(random:uniform(length(List)) - 1, List), 

  % add the head of 2nd part of the splitted list to the result list
  % then merge the remaining cards and shuffle recursively
  shuffle(Leading ++ T, [H | Acc]).