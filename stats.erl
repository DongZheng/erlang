-module(stats).
-export([minimum/1]).
-export([maximum/1]).
-export([range/1]).
-export([mean/1]).
-export([stdv/1]).
-export([sum/1, sum_of_squares/1]).

minimum(List) -> 
  try 
    [H|_] = List,
    minimum(List, H)
  catch 
    error:Error -> {error, badarg}
  end.

minimum([], C) -> C;

minimum([H|T], C) ->
  if 
  	H < C -> minimum(T, H);
  	H >= C -> minimum(T, C)
  end.

maximum(List) ->
  [H|_] = List,
  maximum(List, H).

maximum([], C) -> C;

maximum([H|T], C) ->
  if 
  	H < C -> maximum(T, C);
  	H >= C -> maximum(T, H)
  end.

range(List) ->
  [H|_] = List,
  [minimum(List, H)|[maximum(List, H)]].

sum(List) ->
  lists:foldl(fun(X, Y) -> X + Y end, 0, List).

mean(List) -> 
  try
    sum(List)/length(List)
  catch
    error:Error -> {error, badarith}
  end.

sum_of_squares(List) ->
  lists:foldl(fun(X, Y) -> X * X + Y end, 0, List).

stdv(List) -> 
  try
    N = length(List),
    Sum = sum(List),
    math:sqrt((sum_of_squares(List) * N - Sum * Sum) / (N * (N - 1)))
  catch 
    error:Error -> {error, badarith}
  end.
