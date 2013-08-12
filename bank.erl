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
  	  if 
  	  	Amount < 0 -> 
  	  	  io:format("Deposit may not be less than zero."),
  	  	  error_logger:error_msg("Negative Deposit amount ~w~n", [Amount]),
  	  	  account(Balance);
  	  	Amount > 10000 -> 
  	  	  io:format("Your deposit of $~w may be subject to hold.~n", [Amount]),
  	  	  error_logger:warning_msg("Excessive deposit ~w~n", [Amount]),
  	  	  NewBalance = Balance + Amount,
  	  	  io:format("Your new balance is ~w~n", [NewBalance]),
  	  	  account(NewBalance);
  	  	Amount >= 0 -> calc(deposit, Balance, Amount)
  	  end;
  	$W -> 
  	  Amount = get_number("withdraw"),
  	  if 
  	  	Amount < 0 -> 
  	  	  io:format("Withdrawals may not be less than zero."),
  	  	  error_logger:error_msg("Negative withdrawal amount ~w~n", [Amount]),
  	  	  account(Balance);
  	  	Amount > Balance -> 
  	  	  io:format("You cannot withdraw more than your current balance of ~w.~n", [Balance]),
  	  	  error_logger:error_msg("Overdraw ~w from balance ~w~n", [Amount, Balance]),
  	  	  account(Balance);
  	  	Amount >= 0 -> calc(withdraw, Balance, Amount)
  	  end;
  	$Q -> io:format("true~n");
  	$B -> 
  	  io:format("Balance inquiry ~w~n", [Balance]),
  	  account(Balance);
  	_ -> 
  	  io:format("Unknown command ~s~n", [C]),
  	  account(Balance)
  end.

% Returns the number that was input, accept either integers or floats.
get_number(Prompt) ->
  Str = io:get_line("Amount to " ++ Prompt ++ ": "),
  % string:to_float requires a decimal point; if you just enter input like "3", 
  % you will receive {error, no_float} for your efforts.
  % You should try to convert to float first, 
  {N, _} = string:to_float(Str),
  case N of 
  	% if that fails, try a conversion to integer
  	error ->
  	  {M, _} = string:to_integer(Str),
  	  M;
  	_ -> N
  end.

calc(deposit, Balance, Amount) -> 
  NewBalance = Balance + Amount,
  io:format("Your new balance is ~w~n", [NewBalance]),
  error_logger:info_msg("Successful deposit ~w~n", [Amount]),
  account(NewBalance);
calc(withdraw, Balance, Amount) -> 
  NewBalance = Balance - Amount,
  io:format("Your new balance is ~w~n", [NewBalance]),
  error_logger:info_msg("Successful withdrawal ~w~n", [Amount]),
  account(NewBalance).

