-module(non_fp).
-export([generate_teeth/2]).

% e.g., non_fp:generate_teeth("FTTFTTTT", 0.5).
% when you run this function with a long string (about length > 24) as the 1st param, 
% your output will not show the entire list, don't freak out

% Present is a string consisteing of 'T' and 'F'
% 'T' - tooth is present, 'F' - o.w.
% Probability is a floating point between 0 and 1.0
generate_teeth(Present, Probability) -> generate_teeth(Present, Probability, []).

generate_teeth([], Probability, Result) -> lists:reverse(Result);

generate_teeth([H|T], Probability, Result) -> 
  % refer to individual character as $T or $F
  A = 
    if 
   	  H == $F -> [0];
  	  H == $T -> generate_tooth(Probability)
    end,
  io:format("H = ~w, A = ~w~n", [H, A]),
  generate_teeth(T, Probability, [A | Result]).

generate_tooth(Probability) -> 
  % generate a random number between 0 and 1
  random:seed(now()),
  R = random:uniform(),
  % io:format("Random number between 0 and 1 =~w~n", [R]),
  BaseDepth = 
    if 
  	  R < Probability -> 2;
  	  R >= Probability -> 3
    end,
  % io:format("BaseDepth=~w~n", [BaseDepth]),
  generate_tooth(BaseDepth, 6, []).  

generate_tooth(List, 0, Result) -> Result;

generate_tooth(Base, Count, Result) -> 
  % generate a random integer between -1 and 1, and adds it to the Base
  NextBase = Base + random:uniform(3) - 2,
  % io:format("Base=~w, NextBase=~w~n", [Base, NextBase]),
  generate_tooth(Base, Count - 1, [NextBase | Result]).
