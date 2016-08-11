-module(ulid).

-define(CHARS, <<"0123456789ABCDEFGHJKMNPQRSTVWXYZ">>).
-define(CHAR_LENGTH, 32).
-define(TIME_LENGTH, 10).
-define(RANDOM_BYTES, 10).

%% API exports
-export([
  new/0,
  generate/0,
  generate/1,
  generate_list/0,
  generate_list/1
]).

%%====================================================================
%% API functions
%%====================================================================

-type ulid_generator() :: {non_neg_integer(), [byte()]}.

-spec new() -> ulid_generator().
new() ->
  {system_time(), encode_bytes(rand_bytes())}.

-spec generate() -> binary().
generate() ->
  {_, Ulid} = generate({0,[]}),
  Ulid.

-spec generate(ulid_generator()) -> {ulid_generator(), binary()}.
generate(UlidGenerator) ->
  {NewUlidGenerator, Ulid} = generate_list(UlidGenerator),
  {NewUlidGenerator, erlang:list_to_binary(Ulid)}.

-spec generate_list() -> [byte()].
generate_list() ->
  {_, Ulid} = generate_list({0,[]}),
  Ulid.

-spec generate_list(ulid_generator()) -> {ulid_generator(), [byte()]}.
generate_list({OldSystemTime, OldEncodedBytes}) ->
  SystemTime = system_time(),
  NewEncodedBytes = case SystemTime of
    OldSystemTime -> rotate(OldEncodedBytes);
    _ -> encode_bytes(rand_bytes())
  end,
  {{SystemTime, NewEncodedBytes}, generate_from_time_and_encoded_rand_bytes(SystemTime, NewEncodedBytes)}.

%%====================================================================
%% Internal functions
%%====================================================================

generate_from_time_and_encoded_rand_bytes(SystemTime, EncodedRandBytes) ->
  [to_char(N) || N <- encode_time(SystemTime) ++ EncodedRandBytes].

system_time() ->
  erlang:system_time(milli_seconds).

rand_bytes() ->
  <<_:1, Rest/bitstring>> = crypto:strong_rand_bytes(?RANDOM_BYTES),
  <<0:1, Rest/bitstring>>.

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

rotate(Bytes) ->
  {Res, _} = lists:mapfoldr(fun(Byte, Rotated) ->
    case Rotated of
      true -> {Byte, Rotated};
      false -> case Byte + 1 of
        ?CHAR_LENGTH -> {0, false};
        _ -> {Byte + 1, true}
      end
    end
  end, false, Bytes),
  Res.

