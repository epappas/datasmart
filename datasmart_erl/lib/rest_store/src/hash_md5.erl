%%%-------------------------------------------------------------------
%%% @author evangelosp
%%% @copyright (C) 2014, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 24. Jun 2014 23:48
%%%-------------------------------------------------------------------
-module(hash_md5).
-author("evangelosp").

%% API
-export([build/1]).

build(Str) ->
  Md5Bin =  erlang:md5(Str),
  Md5List = binary_to_list(Md5Bin),
  lists:flatten(list2hex(Md5List)).

list2hex(List) -> lists:map(fun(X) -> int2hex(X) end, List).

int2hex(N) when N < 256 -> [hex(N div 16), hex(N rem 16)].

hex(N) when N < 10 -> $0+N;

hex(N) when N >= 10, N < 16 -> $a + (N-10).