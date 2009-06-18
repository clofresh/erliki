-module(erliki).
-export([start/0, stop/0]).

ensure_started(App) ->
    case application:start(App) of
        ok ->
            ok;
        {error, {already_started, App}} ->
            ok
    end.
        
%% @spec start() -> ok
%% @doc Start the erliki server.
start() ->
    erliki_deps:ensure(),
    ensure_started(crypto),
    application:start(erliki).

%% @spec stop() -> ok
%% @doc Stop the erliki server.
stop() ->
    Res = application:stop(erliki),
    application:stop(crypto),
    Res.
