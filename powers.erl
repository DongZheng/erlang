-module(powers).
-export([raise/2, nth_root/2]).

raise(X, N) -> 
  if 
  	N == 0 -> 1;
  	N > 0 -> raise(X, N, 1);
  	N < 0 -> 1.0 / raise(X, -N)
  end.

raise(X, N, A) ->
  if
  	N == 0 -> A;
  	% every if must find some true statement
  	% there seems no != operator
  	true -> raise(X, N - 1, X * A)
  end.

nth_root(X, N) ->
  % the first approximation is X / 2.0
  nth_root(X, N, X / 2.0).

% A is the approximation to the result
nth_root(X, N, A) ->
  io:format("Current guess is ~w ~n", [A]),
  F = raise(A, N) - X,
  Fprime = N * raise(A, N - 1),
  Next = A - F / Fprime,
  Change = abs(Next - A), % auto-imported BIF erlang:abs/1
  if
  	Change < 1.0e-8 -> Next;
  	true -> nth_root(X, N, Next)
  end.

