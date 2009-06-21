%%
%% wiki_model module
%% Controls how wiki data is read and written from Mnesia
%%
-module(wiki_model).
-author('Carlo Cabanilla <http://syntacticbayleaves.com>').

-export([get_page/1, update_page/3, output_page/2, output/2]).
-include("erliki_records.hrl").
-include_lib("stdlib/include/qlc.hrl").    


get_page(Page) -> 
    mnesia:transaction(fun() ->
        qlc:e(qlc:q(
            [W || W <- mnesia:table(wiki), 
                W#wiki.page =:= Page]
        ))
    end).

update_page(Page, Body, User_Name) ->
    TokenizedBody = wiki_tokenizer:tokenize(Body),
    {ok, Parsed} = wiki_parser:parse(TokenizedBody),
    Timestamp = erlang:localtime(),
    
    case is_tuple(Parsed) of
        true -> 
            Record = #wiki{
                        id={Page, Timestamp},
                        page=Page, 
                        body=[Parsed],
                        author=User_Name,
                        timestamp=Timestamp
                     };
        _ ->
            Record = #wiki{
                        id={Page, Timestamp},
                        page=Page, 
                        body=lists:flatten(Parsed),
                        author=User_Name,
                        timestamp=Timestamp
                     }
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
    end.
    
        

output_page(Page, F) ->
    case wiki_model:get_page(Page) of
        {atomic, []} -> 
            io:format("Could find page: ~p ~n", [Page]),
            {not_found, {Page}};

        {atomic, Result_Set} ->
            [ #wiki{id=Id, page=Page, body=Parsed, author=Last_Author, timestamp=Timestamp} | _ ] = lists:reverse(lists:keysort(2, Result_Set)),
            
            io:format("Found record for ~p ~n", [Page]),
            
            {ok, {Page, F(Parsed), Last_Author}};

        Error -> 
            io:format("Could not get page: ~p ~n", [Error]),
            {not_found, {Page}}
    end.
    

output(Page, wiki) ->
    F = fun(Parsed) ->
            lists:concat(
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
               )
    end,
    
    output_page(Page, F);

output(Page, html) ->
    F = fun(Parsed) ->
            lists:concat(
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
               )
    end,
    
    output_page(Page, F).



