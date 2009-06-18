-module(wiki_tokenizer).
-export([tokenize/1]).

tokenize(S) ->
    lists:flatten(tokenize(S, 0, string_state)).

tokenize(S, Count, State) ->
    case State of
        string_state ->
            Transitions = [{"\\[\\[", '[[', wikilink_state}, 
                           {"<",      '<',  State}, 
                           {">",      '>',  State}];
        wikilink_state ->
            Transitions = [{"\\]\\]", ']]', string_state}, 
                           {"<",      '<',  State}, 
                           {">",      '>',  State}]
    end,
    
    BoundaryPattern = string:join([Boundary || {Boundary, _, _} <- Transitions], "|"),
    
    case regexp:first_match(S, BoundaryPattern) of
        {match, Start, Length} ->
            {Boundary, Token, Next} = hd([{B, T, N} || {B, T, N} <- Transitions,
                    atom_to_list(T) =:= string:substr(S, Start, Length)]),

            [{string, Count + 1, string:substr(S, 1, Start - 1)}, 
             {Token, Count + 2}, 
             tokenize(string:substr(S, Start + Length), Count + 2, Next)];
        nomatch -> 
            [{string, Count + 1, S}]
    end.

