-module(phone_mnesia).
-export([setup/2]).
-include("phone_records.hrl").
-import(dates, [date_parts/1]).

% create the Mnesia tables for the two input files
setup(PhoneFile, CustomerFile) ->
  % create a schema
  mnesia:create_schema([node()]),
  % start Mnesia
  mnesia:start(),

  % High-order function to create customer record from one line of data
  Create_customer_record = fun(Line) -> 
    % option {return, list} is a must, otherwise N will be of type iodata rather than string, looks like <<"650-555-3326">>
    [N, LN, FN, MN, RPPM] = re:split(Line, "[,]", [{return, list}]),
    #customer{number=N, last_name=LN, first_name=FN, middle_name=MN, rate_paid_per_minute=RPPM}
  end,

  % High-order function to create phone record from one line of data
  Create_phone_record = fun(Line) -> 
    % option {return, list} is a must, otherwise N will be of type iodata rather than string, looks like <<"650-555-3326">>
    [N, SD, ST, ED, ET] = re:split(Line, "[,]", [{return, list}]),
    Start = convert_datetime(SD, ST),
    End = convert_datetime(ED, ET),
    #phone{number=N, starting_time=Start, end_time=End}
  end,
    
  % create a table based on the phone record type and load phone data file into mnesia
  fill_table(phone, PhoneFile, Create_phone_record, record_info(fields, phone), bag),

  % create a table based on the customer record type and load customer data file into mnesia
  fill_table(customer, CustomerFile, Create_customer_record, record_info(fields, customer), set).

% TableName - atom, phone or customer
% FileName - string
% Fun - high-order function to adds the data
% RecordInfo - record_info for the field
% TableType - phone call data is a bag, customer data is a set
fill_table(TableName, FileName, Fun, RecordInfo, TableType) ->
  mnesia:create_table(TableName, [{attributes, RecordInfo}, {type, TableType}]),
  case file:open(FileName, [read]) of
    {error, enoent} -> io:format("No such file: ~s~n", [FileName]);
    {_, InputFile} -> 
      F = fun() -> 
        read_line(InputFile, Fun)
      end,
      
      % F can NOT have parameters, o.w., there'll be error: {aborted, no_transaction} 
      % (meaning "Operation not allowed outside transactions."")
      mnesia:transaction(F)
  end.

% recursively read next line from a file, construct a record based on the line, then write the record into mnesia
% File - file
% Fun - high-order function to construct a record based on the line string
read_line(File, Fun) -> 
  case io:get_line(File, "") of
    eof -> ok;
    Line -> 
      Record = Fun(Line),
      mnesia:write(Record),
      read_line(File, Fun)
  end.

% convert a date and time to the number of seconds since the year zero
convert_datetime(DateString, TimeString) ->
  [Y, M, D] = dates:date_parts(DateString, "-"),
  % io:format("~s -> ~w, ~w, ~w~n", [DateString, Y, M, D]),
  [HH, MM, SS] = dates:date_parts(TimeString, ":"),
  % io:format("~s -> ~w, ~w, ~w~n", [TimeString, HH, MM, SS]),
  calendar:datetime_to_gregorian_seconds({list_to_tuple([Y, M, D]), list_to_tuple([HH, MM, SS])}).
