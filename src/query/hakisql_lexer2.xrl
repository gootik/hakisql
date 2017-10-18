Definitions.

D   = [0-9]
L   = [A-Za-z][[A-Za-z0-9_]*
WS  = ([\000-\s]|%.*)
OPERATOR = (<|<=|=|=>|>|!=)

Rules.

in         : {token, {'in', TokenLine, atom(TokenChars)}}.
has        : {token, {'has', TokenLine, atom(TokenChars)}}.
or         : {token, {'or', TokenLine, atom(TokenChars)}}.
and        : {token, {'and', TokenLine, atom(TokenChars)}}.

{OPERATOR} : {token, {op, TokenLine, atom(TokenChars)}}.

'{L}+'     : {token, {string, TokenLine, strip(TokenChars,TokenLen)}}.

{L}+       : {token, {var, TokenLine, atom(TokenChars)}}.

{D}+       : {token, {integer, TokenLine, list_to_integer(TokenChars)}}.

[(),]      : {token, {atom(TokenChars), TokenLine}}.

{WS}+      : skip_token.

Erlang code.

atom(TokenChars) ->
    list_to_atom(string:to_lower(TokenChars)).

strip(TokenChars,TokenLen) ->
    lists:sublist(TokenChars, 2, TokenLen - 2).