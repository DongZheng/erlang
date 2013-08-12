-module(bank).
-export([account/1, get_number/1, calc/3]).

% Balance is numeric
% returns the current balance in the account in imaginary dollars
account(Balance) ->
  % C is a string
  C = io:get_line("D)eposit, W)ithdraw, B)alance, Q)uit: "),

  % get first character of a string
  Action = hd(C),

  case Action of 
  	$D -> 
  	  Amount = get_number("deposit"),
  	  calc(deposit, Balance, Amount);
  	$W -> 
  	  Amount = get_number("withdraw"),
  	  calc(withdraw, Balance, Amount);
  	$Q -> io:format("true~n");
  	$B -> io:format("Balance inquiry ~w~n", [Balance]);
  	_ -> io:format("Unknown command ~s~n", [C])
  end.

get_number(Prompt) ->
  Str = io:get_line("Amount to " ++ Prompt ++ ": "),
  {N, _} = string:to_integer(Str),
  N.

calc(deposit, Balance, Amount) -> io:format("Your new balance is ~w~n", [Balance + Amount]);
calc(withdraw, Balance, Amount) -> io:format("Your new balance is ~w~n", [Balance - Amount]).

