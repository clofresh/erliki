%%
%% Sample default "/" controller, implement this to your needs
%%
-module(wiki_model).

-export([get_page/1, update_page/3, output_page/2, output/2]).
-include("erliki_records.hrl").


get_page(Page) -> 
    mnesia:transaction(fun() ->
        mnesia:select(wiki, 
            [{#wiki{page=Page, body='$1', last_author='$2'}, [], [{{Page, '$1', '$2'}}]}]
        )
    end).

update_page(Page, Body, User_Name) ->
    TokenizedBody = wiki_tokenizer:tokenize(Body),
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
    end.
    
        

output_page(Page, F) ->
    case wiki_model:get_page(Page) of
        {atomic, [ {Page, Parsed, Last_Author} | _ ]} ->
            io:format("Found record for ~p ~n", [Page]),
            
            {ok, {Page, F(Parsed), Last_Author}};
        Error -> 
            io:format("Error: ~p ~n", [Error]),
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



