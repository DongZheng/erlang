-module(dijkstra).
-export([gcd/2]).

gcd(M, N) -> 
  if 
  	M == N -> M;
  	M > N -> gcd(M - N, N);
  	M < N -> gcd(M, N - M)
  end.

