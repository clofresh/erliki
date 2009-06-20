%%
%% Sample default "/" controller, implement this to your needs
%%
-module(wiki_controller,[Env]).

-export([handle_request/2, before_filter/0]).
-include("erliki_records.hrl").

handle_request("index", []) ->
	{redirect, "/wiki/show/main"};
	
handle_request("show", [Page]) ->
   
    case wiki_model:output(Page, html) of
        {ok, Wiki_Record} ->
            {Page, Body, Last_Author} = Wiki_Record,
            
            {render, "wiki/index.html", 
                [{body, Body},
                 {page, Page},
                 {last_author, Last_Author}]};

        {not_found, _} -> 
            io:format("Could not find record for ~p ~n", [Page]),
            {redirect, lists:concat(["/wiki/edit/", Page])};

        Weird -> 
            io:format("~p: ~p~n", [Page, Weird]),

            {error, "Blerg"}
    end;

handle_request("edit", [Page]) ->
    handle_request("edit", [Page], beepbeep_args:method(Env)).

handle_request("edit", [Page], 'GET') ->

    case wiki_model:output(Page, wiki) of
        {ok, Wiki_Record} ->
            {Page, Body, Last_Author} = Wiki_Record;
            
        {not_found, _} -> 
            io:format("Could not find record for ~p ~n", [Page]),
            {Body, Last_Author} = {"", ""};

        _ -> 
            io:format("~p: should not happen~n", [Page]),
            Body = "",
            {error, "Blerg"}
    end,
    
    {render, "wiki/edit.html", 
        [{body, Body},
         {action, lists:concat(["/wiki/edit/", Page])}]};

handle_request("edit", [Page], 'POST') ->
    Body = beepbeep_args:get_param("body", Env),
    User_Name = beepbeep_args:get_session_data("user_name", Env),
    
    wiki_model:update_page(Page, Body, User_Name),

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

