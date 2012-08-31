%% @doc: Middleware adding the "Date" HTTP header to Elli normal
%% responses. Errors handled by Elli itself will not have the "Date"
%% header. The value is cached and updated once a second by
%% elli_date_server

-module(elli_date).
-behaviour(elli_handler).
-export([handle/2, handle_event/3]).
-export([postprocess/3]).

%%
%% ELLI CALLBACKS
%%
handle(_, _) ->
    ignore.

handle_event(elli_startup, [], _Config) ->
    elli_date_server:start_link(),
    ok;
handle_event(_, _, _) ->
    ok.

%%
%% ELLI MIDDLEWARE CALLBACKS
%%
postprocess(Req, {ResponseCode, Body}, Config)
  when is_integer(ResponseCode) orelse ResponseCode =:= ok ->
    postprocess(Req, {ResponseCode, [], Body}, Config);

postprocess(_Req, {ResponseCode, Headers, Body}, _Args)
  when is_integer(ResponseCode) orelse ResponseCode =:= ok ->
    {ResponseCode, [{<<"Date">>, rfc1123()} | Headers], Body};

postprocess(_, Res, _) ->
    Res.

%%
%% INTERNAL
%%

rfc1123() ->
    case ets:lookup(elli_date, rfc1123) of
        [{rfc1123, Date}] ->
            Date;
        [] ->
            <<"">>
    end.
