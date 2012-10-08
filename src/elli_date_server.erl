%% @doc: Updates the cached date in the ETS-table
-module(elli_date_server).
-behaviour(gen_server).

%% API
-export([start_link/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).


-record(state, {}).

%%%===================================================================
%%% API
%%%===================================================================


start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).


%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
    elli_date = ets:new(elli_date, [named_table, protected]),
    handle_info(update_date, #state{}),
    timer:send_interval(1000, update_date),
    {ok, #state{}}.

handle_call(_, _From, State) ->
    {reply, ok, State}.

handle_cast(_, State) ->
    {noreply, State}.


handle_info(update_date, State) ->
    Date = list_to_binary(httpd_util:rfc1123_date()),
    ets:insert(elli_date, {rfc1123, Date}),
    {noreply, State}.


terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

