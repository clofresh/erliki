%%
%% user_model module
%% Handles user creation and authentication
%%
-module(user_model).
-author('Carlo Cabanilla <http://syntacticbayleaves.com>').

-export([create/2, authenticate/2]).

-include("erliki_records.hrl").
-include_lib("stdlib/include/qlc.hrl").    

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
    


