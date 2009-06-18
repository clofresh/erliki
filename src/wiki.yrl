Header "%% Erliki".
Nonterminals phrase wikilink word.
Terminals string '[[' ']]' '<' '>'.
Rootsymbol phrase.

phrase -> word : '$1'.
phrase -> word phrase : ['$1', '$2'].
wikilink -> '[[' string ']]' : {'wikilink', '$2'}.
word -> wikilink : '$1'.
word -> string : '$1'.
word -> '<' : '$1'.
word -> '>' : '$1'.
