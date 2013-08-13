-module(ask_area).
-export([area/0, get_dimensions/2]).
-import(geom, [area/3]).

area() -> 
  % Don't use get_chars/2 here, otherwise the proceeding get_line/1 will report error
  I = io:get_line("R)ectangle, T)riangle, or E)llipse >"),
  C = hd(I),
  S = char_to_shape(C),
  Numbers = case S of 
    % you can't write '"Unknown shape " ++ C' since C is a char, not a string
  	unknown -> {error, "Unknown shape " ++ I};
  	rectangle -> get_dimensions("width", "height");
  	triangle -> get_dimensions("base", "height");
    ellipse -> get_dimensions("major axis", "minor axis")
  end,
  % io:format("Numbers = ~p~n", [Numbers]),
  calculate(S, element(1, Numbers), element(2, Numbers)).

% Given a character parameter, return an atom representing the specified shape
char_to_shape(C) ->
  case C of
  	$R -> rectangle;
    $r -> rectangle;
    $T -> triangle;
    $t -> triangle;
    $E -> ellipse;
    $e -> ellipse;
    _ -> unknown
  end.

% Returns the number that was input, accept either integers or floats.
get_number(Prompt) ->
  Str = io:get_line("Enter " ++ Prompt ++ " > "),
  % string:to_float requires a decimal point; if you just enter input like "3", 
  % you will receive {error, no_float} for your efforts.
  % You should try to convert to float first, 
  {N, _} = string:to_float(Str),
  case N of 
    % if that fails, try a conversion to integer
    error ->
      {M, _} = string:to_integer(Str),
      case M of 
        % if still fails, return error atom
        error -> error;
        _ -> M
      end;
    _ -> N
  end.

% Takes two prompts as its parameters (one for each dimension), calls get_number/1 twice.
% Returns a tuple {N1, N2} with the dimensions.
get_dimensions(P1, P2) -> 
  D1 = get_number(P1),
  % io:format(P1 ++ "=~w~n", [D1]),
  D2 = get_number(P2),
  % io:format(P2 ++ "=~w~n", [D2]),
  {D1, D2}.

% Take a shape (as an atom) and two dimensions as its parameters.
% If the shape is unknown, displays error msg
calculate(unknown, _, Err) -> io:format("~s~n", [Err]);
% If either dimension is not numeric, or is negative numeric, displays error msg
calculate(_, error, _) -> io:format("Error in first number~n");
calculate(_, _, error) -> io:format("Error in second number~n");
% Calculate the area of the shape
calculate(S, N1, N2) -> 
  if 
    N1 < 0; N2 < 0 -> io:format("Both numbers must be greater than or equal to zero.~n");
    true -> geom:area(S, N1, N2)
  end.
