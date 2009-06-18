%%
%% Sample default "/" controller, implement this to your needs
%%
-module(home_controller,[Env]).

-export([handle_request/2]).

handle_request("index",[]) ->
    {redirect,"/wiki/"}.


