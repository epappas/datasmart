%%%-------------------------------------------------------------------
%%% @author evangelosp
%%% @copyright (C) 2014, evalonlabs
%%% Copyright 2015, evalonlabs
%%%
%%% Licensed under the Apache License, Version 2.0 (the 'License');
%%% you may not use this file except in compliance with the License.
%%% You may obtain a copy of the License at
%%%
%%%     http://www.apache.org/licenses/LICENSE-2.0
%%%
%%% Unless required by applicable law or agreed to in writing, software
%%% distributed under the License is distributed on an 'AS IS' BASIS,
%%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%%% See the License for the specific language governing permissions and
%%% limitations under the License.
%%% @doc
%%%
%%% @end
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