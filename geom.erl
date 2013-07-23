%% @author Dong <nicole.zheng.nj@hotmail.com>
%% @doc Function calculating rectangle area

-module(geom).
-export([area/3]).

area(S, A, B) when A >= 0, B >= 0 -> 
  case S of 
	rectangle -> A * B;
	triangle -> A * B / 2.0;
	ellipse -> math:pi() * A * B
  end.
