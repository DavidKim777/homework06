-module(homework06).

-behavior(gen_server).

-export([start_link/0,
    stop/0,
    init/1]).

-export([handle_call/3,
    handle_cast/2]).

-export([insert/2,
    insert/3,
    lookup/1,
    delete/1]).

-export([create/0,
    insert_ets/2,
    insert_ets/3,
    lookup_ets/1,
    delete_ets/1]).

-record(cache, {value, expire_time}).

-define(TABLE_NAME, my_table).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

stop() ->
    gen_server:cast(?MODULE, stop).

init([]) ->
    create(),
    {ok, #{}}.

insert(Key, Value) ->
    gen_server:cast(?MODULE, {insert, Key, Value}).

insert(Key, Value, Ttl) ->
    gen_server:cast(?MODULE, {insert, Key, Value, Ttl}).

lookup(Key) ->
    gen_server:call(?MODULE, {lookup, Key}).

delete(Key) ->
    gen_server:cast(?MODULE, {delete, Key}).

create() ->
    ets:new(?TABLE_NAME, [set, public, named_table]).

insert_ets(Key, Value) ->
    ets:insert(?TABLE_NAME, {Key, #cache{value = Value, expire_time = undefined}}),
    ok.

insert_ets(Key, Value, Ttl) ->
    TimeNow = calendar:universal_time(),
    CurrentTime = calendar:datetime_to_gregorian_seconds(TimeNow),
    ExpireAt = CurrentTime + Ttl,
    ets:insert(?TABLE_NAME, {Key, #cache{value = Value, expire_time = ExpireAt}}),
    ok.

lookup_ets(Key) ->
    TimeNow = calendar:universal_time(),
    CurrentTime = calendar:datetime_to_gregorian_seconds(TimeNow),
    Res = ets:lookup(?TABLE_NAME, Key),
    io:format("ETS Lookup Result for ~p: ~p~n", [Key, Res]),
    case Res of
        [{Key, #cache{value = Value1, expire_time = undefined}}] ->
            {ok, Value1};
        [{Key, #cache{value = Value1, expire_time = ExpireTime}}] when is_integer(ExpireTime), ExpireTime > CurrentTime ->
            {ok, Value1};
        _ ->
            {error, not_found}
    end.

delete_ets(Key) ->
    ets:delete(?TABLE_NAME, Key),
    ok.

handle_call({lookup, Key}, _From, State) ->
    Value = lookup_ets(Key),
    {reply, Value, State}.

handle_cast({insert, Key, Value}, State) ->
    insert_ets(Key, Value),
    {noreply, State};
handle_cast({insert, Key, Value, Ttl}, State) ->
    insert_ets(Key, Value, Ttl),
    {noreply, State};
handle_cast({delete, Key}, State) ->
    delete_ets(Key),
    {noreply, State};
handle_cast(stop, State) ->
    ets:delete_all_objects(?TABLE_NAME),
    {noreply, State}.