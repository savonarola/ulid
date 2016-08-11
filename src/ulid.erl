-module(ulid).

-define(CHARS, <<"0123456789ABCDEFGHJKMNPQRSTVWXYZ">>).
-define(CHAR_LENGTH, 32).
-define(TIME_LENGTH, 10).
-define(MAX_TIME, 1125899906842624).
-define(RANDOM_BYTES, 10).

%% API exports
-export([generate/0, generate_list/0]).

%%====================================================================
%% API functions
%%====================================================================

-spec generate() -> binary().

generate() ->
  erlang:list_to_binary(generate_list()).

-spec generate_list() -> list().

generate_list() ->
  [ to_char(N) || N <- encode_time(system_time()) ++ encode_bytes(rand_bytes())].

%%====================================================================
%% Internal functions
%%====================================================================

system_time() ->
  erlang:system_time(milli_seconds).

rand_bytes() ->
  crypto:rand_bytes(?RANDOM_BYTES).

encode_time(N) -> encode_time(N, ?TIME_LENGTH, []).

encode_time(_, 0, L) -> L;
encode_time(N, NBytes, L) ->
  Mod = N rem ?CHAR_LENGTH,
  NNew = N div ?CHAR_LENGTH,
  encode_time(NNew, NBytes - 1, [Mod | L]).

encode_bytes(Bytes) -> encode_bytes(Bytes, []).
encode_bytes(<<>>, Chars) -> Chars;
encode_bytes(<<C:5, Bytes/bitstring>>, Chars) -> encode_bytes(Bytes, [C | Chars]).

to_char(N) when (N >= 0) and (N =< 31) ->
  <<_:N/binary,C:8,_/binary>> = ?CHARS,
  C.


