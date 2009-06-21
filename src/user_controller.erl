%%
%% user_controller module
%% Controls how web requests to /user/* are processed
%%
-module(user_controller,[Env]).
-author('Carlo Cabanilla <http://syntacticbayleaves.com>').

-export([handle_request/2]).
-include("erliki_records.hrl").
-include_lib("stdlib/include/qlc.hrl").    

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

    case authenticate(User_Name, Password) of
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
    
    case create(Name, Password1) of
        {atomic, _} ->
            beepbeep_args:set_session_data("user_name", Name, Env),
            {redirect, "/wiki/"};
        _ ->
            io:format("error!~n", []),
            {redirect, "/error/"}
    end.

create(Name, Password) ->
    io:format("Creating user: (~p, ~p) ~n", [Name, Password]),
    Record = #user{
                name=Name, 
                password_hash=sha2:hexdigest256(lists:concat([Password, salt()]))
            },

    io:format("Hash is ~p~n", [Record#user.password_hash]),

    mnesia:transaction(fun() -> 
        mnesia:write(Record) 
    end).

authenticate(Name, Password) ->
    Password_Hash = sha2:hexdigest256(lists:concat([Password, salt()])),
    io:format("Name: ~p, Password: ~p, Password_Hash: ~p ~n", [Name, Password, Password_Hash]),
    case mnesia:transaction(fun() ->
        qlc:e(qlc:q([U || U <- mnesia:table(user), 
                        U#user.name =:= Name,
                        U#user.password_hash =:= Password_Hash]))
    end) of
        {atomic, _} ->
            ok;
        Weird ->
            io:format("~p ~n", [Weird]),
            not_ok
    end.

salt() ->
    "?8j#$&&zTXD^priV<bFi(EW^[(:X\'vk[".
    


