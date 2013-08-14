-module(dates).
-export([date_parts/2]).
-export([julian/1]).

date_parts(S, Delimiter) ->
  % re:split/3 here returns a list of strings
  [Y, M, D] = re:split(S, "[" ++ Delimiter ++ "]", [{return, list}]),
  
  % string:to_integer/1 returns a tuple, including the integer contents of the string, plus leftovers.
  % e.g., 
  % 9> string:to_integer("3").
  % {3,[]}

  % built-in function element/2, takes a numeric position (starting from 1) and a tuple as its argument
  [element(1, string:to_integer(Y)),
   element(1, string:to_integer(M)),
   element(1, string:to_integer(D))].

is_leap_year(Year) ->
  (Year rem 4 == 0 andalso Year rem 100 /= 0)
  orelse (Year rem 400 == 0).

julian(S) ->
  DaysPerMonth = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31],
  [Y, M, D] = date_parts(S, "-"),

  % A is the full month to be calculated
  {A, B} = lists:split(M - 1, DaysPerMonth),
  julian(Y, M, D, A, 0).
  
julian(Y, M, D, [], Result) -> 
  % illegal: if is_leap_year(Y) -> 
  % call to local/imported function any_gte_four/1 is illegal in guard
  Is = is_leap_year(Y),
  if 
  	M > 2, Is -> Result + D + 1;
    M =< 2; not Is -> Result + D
  end;

julian(Y, M, D, [H|T], Result) ->
  julian(Y, M, D, T, Result + H).


