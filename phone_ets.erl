-module(phone_ets).
-export([setup/1, summary/1, summary/0]).
-include("phone_records.hrl").
-import(stats, [sum/1]).
-import(dates, [date_parts/1]).

% each time before calling phone_ets:setup("") in erlang shell, 
% call f(). to clear existing table info from memory

% TODO: ets:tab2list/1 and ets:lookup/2 will print all matched records on the console, 
% how to compress the output? or any alternatives to the two functions?

% create an ETS table for phone calls by reading a file
setup(FileName) -> 
  % bag can't be positioned before named_table...
  ets:new(phones, [named_table, bag, {keypos, #phone.number}]),
  % ets:info(PhoneTable).
  load_file(FileName).

load_file(FileName) ->
  case file:open(FileName, [read]) of
  	{error, enoent} -> io:format("No such file: ~s~n", [FileName]);
    {_, InputFile} -> read_line(InputFile)
  end.

% recursively read next line from a file
read_line(File) -> 
  case io:get_line(File, "") of
  	eof -> ok;
  	Line -> 
  	  Record = create_record(Line),
  	  % io:format("~s -> ~s~n", [Line, Record#phone.number]),
  	  ets:insert(phones, Record),
  	  read_line(File)
  end.

% create record from one line of data
create_record(Line) -> 
  % option {return, list} is a must, otherwise N will be of type iodata rather than string, looks like <<"650-555-3326">>
  [N, SD, ST, ED, ET] = re:split(Line, "[,]", [{return, list}]),
  % io:format("~s -> ~s, ~s, ~s, ~s, ~s", [Line, N, SD, ST, ED, ET]),
  Start = convert_datetime(SD, ST),
  End = convert_datetime(ED, ET),
  #phone{number=N, starting_time=Start, end_time=End}.

% convert a date and time to the number of seconds since the year zero
convert_datetime(DateString, TimeString) ->
  [Y, M, D] = dates:date_parts(DateString, "-"),
  % io:format("~s -> ~w, ~w, ~w~n", [DateString, Y, M, D]),
  [HH, MM, SS] = dates:date_parts(TimeString, ":"),
  % io:format("~s -> ~w, ~w, ~w~n", [TimeString, HH, MM, SS]),
  calendar:datetime_to_gregorian_seconds({list_to_tuple([Y, M, D]), list_to_tuple([HH, MM, SS])}).

duration(Record) -> 
  io:format("~s, ~w, ~w~n", [Record#phone.number, Record#phone.starting_time, Record#phone.end_time]),
  (Record#phone.end_time - Record#phone.starting_time + 59) div 60.

sum_of(Number) -> 
  % look for all records match the given Number, returns the list of durations
  Durations = [duration(Record) || Record <- ets:lookup(phones, Number)],
  % calculate the sum of the Durations list and return a tuple, 
  % which first is the Number and second is the sum.
  {Number, stats:sum(Durations)}.

summary(Number) -> 
  [sum_of(Number)].

summary() ->
  AllRecords = ets:tab2list(phones),
  % get a list of phone numbers
  Numbers = [N#phone.number || N <- AllRecords],
  % filter out duplicated numbers
  NumberSet = sets:from_list(Numbers),
  % convert the set to list 
  NumberList = sets:to_list(NumberSet),
  % list comprehension
  [sum_of(N) || N <- NumberList].
