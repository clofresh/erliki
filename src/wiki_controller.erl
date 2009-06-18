%%
%% Sample default "/" controller, implement this to your needs
%%
-module(wiki_controller,[Env]).

-export([handle_request/2, before_filter/0]).
-include("erliki_records.hrl").

handle_request("index", []) ->
	{redirect, "/wiki/show/main"};
	
handle_request("show", [Page]) ->

    case mnesia:transaction(fun() ->
        mnesia:select(wiki, 
            [{#wiki{page=Page, body='$1', last_author='$2'}, [], [{{'$1', '$2'}}]}]
        )
    end) of

        {atomic, [ {Parsed, Last_Author} | _ ]} ->
            io:format("Found record for ~p ~n", [Page]),
            
            Body = lists:concat(
                     lists:map(fun(X) ->
                        case X of
                            {string, _, S} -> 
                                S;
                            {wikilink,{string, _, S}} -> 
                                lists:concat(
                                    [lists:concat(["<a href='/wiki/show/", S, "'>"]), 
                                     S, 
                                     "</a>"]);
                            {'<', _} -> 
                                "&lt;";
                            {'>', _} -> 
                                "&gt;"
                        end
                      end,
                      Parsed
                     )
                   ),
            
            {render, "wiki/index.html", 
                [{body, Body},
                 {page, Page},
                 {last_author, Last_Author}]};

        {atomic, Query} -> 
            io:format("Could not find record for ~p (~p) ~n", [Page, Query]),
            {redirect, lists:concat(["/wiki/edit/", Page])};

        Weird -> 
            io:format("~p: ~p~n", [Page, Weird]),

            {error, "Blerg"}
    end;

handle_request("edit", [Page]) ->
    handle_request("edit", [Page], beepbeep_args:method(Env)).

handle_request("edit", [Page], 'GET') ->
    case mnesia:transaction(fun() ->
        mnesia:select(wiki, 
            [{#wiki{page=Page, body='$1', last_author='$2'}, [], ['$1']}]
        )
    end) of

        {atomic, [ Parsed | _ ]} ->
            io:format("Found record for ~p ~n", [Page]),
            
            Body = lists:concat(
                     lists:map(fun(X) ->
                        case X of
                            {string, _, S} -> 
                                S;
                            {wikilink, {string, _, S}} -> 
                                lists:concat(["[[", S, "]]"]);
                            {'<', _} -> 
                                "<";
                            {'>', _} -> 
                                ">"
                        end
                      end,
                      Parsed
                     )
                   );
            
        {atomic, _} -> 
            io:format("Could not find record for ~p ~n", [Page]),
            Body = "";
        _ -> 
            io:format("~p: should not happen~n", [Page]),
            Body = ""
    end,
    
    {render, "wiki/edit.html", 
        [{body, Body},
         {action, lists:concat(["/wiki/edit/", Page])}]};

handle_request("edit", [Page], 'POST') ->
    Body = beepbeep_args:get_param("body", Env),
    TokenizedBody = wiki_tokenizer:tokenize(Body),
    User_Name = beepbeep_args:get_session_data("user_name", Env),
    {ok, Parsed} = wiki_parser:parse(TokenizedBody),
    
    case is_tuple(Parsed) of
        true -> 
            Record = #wiki{page=Page, 
                           body=[Parsed],
                           last_author=User_Name};
        _ ->
            Record = #wiki{page=Page, 
                           body=lists:flatten(Parsed),
                           last_author=User_Name}
    end,

    case mnesia:transaction(fun() -> 
        mnesia:write(Record) 
    end) of
        {atomic, Result} ->
            io:format("Wrote: ~p ~p ~p ~n", [Page, Body, Result]);
        {aborted, Reason} ->
            io:format("Couldn't write ~p: ~p ~n", [Record, Reason]);
        Weird ->
            io:format("~p ~n", [Weird])
    end,

    {redirect, lists:concat(["/wiki/show/", Page])}.

before_filter() ->
    case beepbeep_args:get_session_data("user_name", Env) of
        undefined ->
             beepbeep_args:set_session_data("user_name", "Anon", Env);
        _ ->
            noop
    end,
    
    io:format("User: ~p ~n", [beepbeep_args:get_session_data("user_name", Env)]),
    
    ok.

