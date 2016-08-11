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
  UG = ulid:new(),

  {UG1, <<_:26/binary>>} = ulid:generate(UG),

  {UG2, U1} = ulid:generate(UG1),
  {UG3, U2} = ulid:generate(UG2),
  timer:sleep(1),
  {_, U3} = ulid:generate(UG3),

  true = U1 < U2,
  true = U2 < U3.


generate_list(_Config) ->
  UG = ulid:new(),

  {UG1, L} = ulid:generate_list(UG),

  true = is_list(L),
  26 = length(L),

  {UG2, U1} = ulid:generate_list(UG1),
  {UG3, U2} = ulid:generate_list(UG2),
  timer:sleep(1),
  {_, U3} = ulid:generate_list(UG3),

  true = U1 < U2,
  true = U2 < U3.
