%%
%% user_controller module
%% Controls how web requests to /user/* are processed
%%
-module(user_controller,[Env]).
-author('Carlo Cabanilla <http://syntacticbayleaves.com>').

-export([handle_request/2]).

handle_request("index", []) ->
    {render,"home/index.html",[{data,"Hello There From BeepBeep!"}]};

handle_request("login", []) ->
    handle_request("login", [], beepbeep_args:method(Env));

handle_request("register", []) ->
    handle_request("register", [], beepbeep_args:method(Env)).

handle_request("login", [], 'GET') ->
    {render, "user/login.html", []};

handle_request("login", [], 'POST') ->
    User_Name = beepbeep_args:get_param("user_name", Env),
    Password  = beepbeep_args:get_param("user_password", Env),

    case user_model:authenticate(User_Name, Password) of
        ok ->
            io:format("Successful login: ~p ~n", [User_Name]),
            beepbeep_args:set_session_data("user_name", User_Name, Env),
            {redirect, "/"};
        not_ok -> 
            io:format("Bad login: ~p/~p ~n", [User_Name, Password]),
            {redirect, "/user/login"}
    end;

handle_request("register", [], 'GET') ->
    {render, "user/register.html", []};

handle_request("register", [], 'POST') ->
    % TODO: validate

    Name = beepbeep_args:get_param("user_name", Env), 
    Password1 = beepbeep_args:get_param("user_password1", Env),
    Password2 = beepbeep_args:get_param("user_password2", Env),
    
    Password1 = Password2,
    
    case user_model:create(Name, Password1) of
        {atomic, _} ->
            beepbeep_args:set_session_data("user_name", Name, Env),
            {redirect, "/wiki/"};
        _ ->
            io:format("error!~n", []),
            {redirect, "/error/"}
    end.

