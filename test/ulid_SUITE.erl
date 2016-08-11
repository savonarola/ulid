-module(ulid_SUITE).

-export([all/0]).
-export([
  generate_0/1,
  generate_1/1,
  generate_list_0/1,
  generate_list_1/1,
  fuzzy/1
]).

all() -> [
  generate_0,
  generate_1,
  generate_list_0,
  generate_list_1,
  fuzzy
].

generate_1(_Config) ->
  UG = ulid:new(),

  {UG1, <<_:26/binary>>} = ulid:generate(UG),

  {UG2, U1} = ulid:generate(UG1),
  {UG3, U2} = ulid:generate(UG2),
  timer:sleep(1),
  {_, U3} = ulid:generate(UG3),

  true = U1 < U2,
  true = U2 < U3.

generate_0(_Config) ->

  <<_:26/binary>> = ulid:generate(),

  U1 = ulid:generate(),
  timer:sleep(1),
  U2 = ulid:generate(),

  true = U1 < U2.

generate_list_1(_Config) ->
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

generate_list_0(_Config) ->

  L = ulid:generate_list(),

  true = is_list(L),
  26 = length(L),

  U1 = ulid:generate_list(),
  timer:sleep(1),
  U2 = ulid:generate_list(),

  true = U1 < U2.

fuzzy(_Config) ->
  UG = ulid:new(),
  {Ulids, _} = lists:foldl(fun(_, {Ulids, UlidGen}) ->
    {NewGen, Ulid} = ulid:generate(UlidGen),
    {[Ulid | Ulids], NewGen}
  end, {[], UG}, lists:seq(1, 1000000)),

  assert_desc(Ulids).

assert_desc([_]) -> true;
assert_desc([]) -> true;
assert_desc([U1, U2 | Rest]) ->
  case U1 > U2 of
    true -> nop;
    false -> throw({U1, U2})
  end,
  true = U1 > U2,
  assert_desc([U2 | Rest]).



