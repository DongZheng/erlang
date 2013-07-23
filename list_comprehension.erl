-module(list_comprehension).
-export([man_over_fourty/1]).
-export([man_or_over_fourty/1]).

% People = [{"Federico", $M, 22}, {"Kim", $F, 45}, {"Hansa", $F, 30},
%     {"Tran", $M, 47}, {"Cathy", $F, 32}, {"Elias", $M, 50}].

% call man_over_fourty(People).
man_over_fourty(List) -> 
  [N || {N, G, A} <- List, G == $M, A > 40].

man_or_over_fourty(List) -> 
  % because or has higher priority than comparison operators, use () here to avoid compilation error
  % ';' can't replace 'or' -- this is different from 'if' syntax (comma for and, semicolon for or)
  [N || {N, G, A} <- List, (G == $M) or (A > 40)].
