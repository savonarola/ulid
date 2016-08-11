-module(ulid_SUITE).

-export([all/0]).
-export([
  generate/1,
  generate_list/1
]).

all() -> [
  generate,
  generate_list
].

generate(_Config) ->
  <<_:26/binary>> = ulid:generate(),

  U1 = ulid:generate(),
  timer:sleep(1),
  U2 = ulid:generate(),

  true = U1 < U2.


generate_list(_Config) ->
  L = ulid:generate_list(),

  true = is_list(L),
  26 = length(L),

  U1 = ulid:generate_list(),
  timer:sleep(1),
  U2 = ulid:generate_list(),

  true = U1 < U2.
