-module(homework06_SUITE).

-compile(export_all).

-include_lib("common_test/include/ct.hrl").

-export([all/0,
    init_per_suite/1,
    end_per_suite/1,
    init_per_testcase/2,
    end_per_testcase/2]).

all() ->
    [create_table, insert_and_lookup, insert_with_ttl].

init_per_suite(Config) ->
    {ok, _} = homework06:start_link(),
    Config.

end_per_suite(_Config) ->
  homework06:stop(),
  ok.

init_per_testcase(_TestCase, Config) ->
    Config.


end_per_testcase(_TestCase, _Config) ->
    ok.

create_table(_Config) ->
    ?assertEqual(ok, homework06:create(my_table)).

insert_and_lookup(_Config) ->
    homework06:create(my_table),
    homework06:insert(my_table, key1, value1),
    ?assertEqual(value1, homework06:lookup(my_table, key1)).

insert_with_ttl(_Config) ->
    homework06:create(my_table),
    homework06:insert(my_table, key2, value2, 1),
    ?assertEqual(value2, homework06:lookup(my_table, key2)),
    timer:sleep(2000),
    ?assertEqual(undefined, homework06:lookup(my_table, key2)).