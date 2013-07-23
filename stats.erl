-module(stats).
-export([minimum/1]).
-export([maximum/1]).
-export([range/1]).

minimum(List) -> 
  [H|_] = List,
  minimum(List, H).

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
