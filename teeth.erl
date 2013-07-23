-module(teeth).
-export([alert/1]).
% -export([any_gte_four/1]).

% PocketDepths = [[0], [2,2,1,2,2,1], [3,1,2,3,2,3],
%     [3,1,3,2,1,2], [3,2,3,2,2,1], [2,3,1,2,1,1],
%     [3,1,3,2,3,2], [3,3,2,1,3,1], [4,3,3,2,3,3],
%     [3,1,1,3,2,2], [4,3,4,3,2,3], [2,3,1,3,2,2],
%     [1,2,1,1,3,2], [1,2,2,3,2,3], [1,3,2,1,3,3], [0],
%     [3,2,3,1,1,2], [2,2,1,1,3,2], [2,1,1,1,1,2],
%     [3,3,2,1,1,3], [3,1,3,2,3,2], [3,3,1,2,3,3],
%     [1,2,2,3,3,3], [2,2,3,2,3,3], [2,2,2,4,3,4],
%     [3,4,3,3,3,4], [1,1,2,3,1,2], [2,2,3,2,1,3],
%     [3,4,2,4,4,3], [3,3,2,1,2,3], [2,2,2,2,3,3],
%     [3,2,3,2,3,2]].

% return true if any of the list is greater than or equal to 4
any_gte_four([]) -> false;

any_gte_four([H|T]) ->
  if 
  	H >= 4 -> true;
  	H < 4 -> any_gte_four(T)
  end.

% call alert(PocketDepths).
alert(List) -> alert(List, 1, []).

alert([], Position, Result) -> lists:reverse(Result);

alert([H|T], Position, Result) -> 
  % illegal: if any_gte_four(H) -> 
  % call to local/imported function any_gte_four/1 is illegal in guard
  A = any_gte_four(H),
  if A -> alert(T, Position + 1, [Position|Result]);
  	not A -> alert(T, Position + 1, Result)
  end.
