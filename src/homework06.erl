-module(homework06).

-behavior(gen_server).

-export([start_link/0,
  stop/0,
  init/1]).

-export([handle_call/3,
  handle_cast/2]).

-export([create/1,
  insert/3,
  insert/4,
  lookup/2]).

-record(cashe, {value, expire_time}).

start_link() ->
  gen_server:start_link([local, ?MODULE], ?MODULE, [], []).

stop() ->
  ok.

init([]) ->
  {ok, #{}}.

create(TableName) ->
  gen_server:call(?MODULE, TableName).

insert(TableName, Key, Value) ->
  gen_server:cast(?MODULE, {TableName, Key, Value}).

insert(TableName, Key, Value, Ttl) ->
  gen_server:cast(?MODULE, {TableName, Key, Value, Ttl}).

lookup(TableName, Key) ->
  gen_server:call(?MODULE, {TableName, Key}).

handle_call({TableName, Key}, _From, State) ->
  TimeNow = calendar:universal_time(),
  CurrentTime = calendar:datetime_to_gregorian_seconds(TimeNow),
  Res = ets:lookup(TableName, Key),
  Value = case Res of
    {Key, #cashe{value = Value1, expire_time = undefined}} ->
      Value1;
    {Key, #cashe{value =  Value1, expire_time = ExpireTime}} when is_integer(ExpireTime), ExpireTime > CurrentTime ->
      Value1;
    _ -> undefined
  end,
  {reply, Value, State};

handle_call(TableName, _From, State) ->
  ok = ets:new(TableName, [set, public, named_table]),
  {reply, ok, State}.

handle_cast({TableName, Key, Value}, State) ->
  ets:insert(TableName, {Key, Value}),
  {noreply, State};
handle_cast({TableName, Key, Value, Ttl}, State) ->
  TimeNow = calendar:universal_time(),
  CurrentTime = calendar:datetime_to_gregorian_seconds(TimeNow),
  ExpireAt = add_seconds(CurrentTime, Ttl),
  ets:insert(TableName, {Key, #cashe{value = Value, expire_time = ExpireAt}}),
  {noreply, State}.

add_seconds(Sec, Seconds) ->
  Sec + Seconds.