-module(erliki_app).

-behaviour(application).
-export([start/2,stop/1]).


%% @spec start(_Type, _StartArgs) -> ServerRet
%% @doc application start callback for erliki.
start(_Type, _StartArgs) ->
    erliki_deps:ensure(),
    erliki_sup:start_link().

%% @spec stop(_State) -> ServerRet
%% @doc application stop callback for erliki.
stop(_State) ->
    ok.
