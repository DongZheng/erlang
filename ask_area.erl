-module(ask_area).
-export([area/0]).
-import(geom, [area/3]).

area() -> 
  C = io:get_chars("R)ectangle, T)riangle, or E)llipse >", 1),
  S = char_to_shape(C),
  io:format("S=~w~n", [S]),
  Numbers = case S of 
  	unknown -> {"error", "Unknown shape " ++ C};
  	rectangle -> get_dimensions("width", "height");
  	triangle -> get_dimensions("base", "height");
    ellipse -> get_dimensions("major axis", "minor axis")
  end,
  calculate(S, element(1, Numbers), element(2, Numbers)).

char_to_shape(C) ->
  case string:to_lower(C) of
  	"r" -> rectangle;
    "t" -> triangle;
    "e" -> ellipse;
    _ -> unknown
  end.

get_number(Prompt) ->
  Str = io:get_line("Enter " ++ Prompt ++ " > "),
  % Value = string:strip(Input, right, $\n),
  {N, _} = string:to_integer(Str),
  N.

get_dimensions(P1, P2) -> 
  io:format("dimension1=" ++ P1 ++ ", dimension2=" ++ P2 ++ "~n"),
  D1 = get_number(P1),
  D2 = get_number(P2),
  {D1, D2}.

calculate(unknown, _, Err) -> io:format("~s~n", [Err]);
calculate(S, N1, N2) -> geom:area(S, N1, N2).
