Definitions.

D   = [0-9]
L   = [A-Za-z0-9][[A-Za-z0-9_]*
WS  = ([\000-\s]|%.*)
OPERATOR = (<|<=|=|=>|>|!=)

Rules.

in         : {token, {'in', TokenLine, atom(TokenChars)}}.
has        : {token, {'has', TokenLine, atom(TokenChars)}}.
or         : {token, {'or', TokenLine, atom(TokenChars)}}.
and        : {token, {'and', TokenLine, atom(TokenChars)}}.
not        : {token, {'not', TokenLine, atom(TokenChars)}}.

{OPERATOR} : {token, {op, TokenLine, atom(TokenChars)}}.

{D}+       : {token, {integer, TokenLine, list_to_integer(TokenChars)}}.
{D}+\.{D}+ : {token, {float, TokenLine, list_to_integer(TokenChars)}}.

'{L}+'     : {token, {string, TokenLine, strip(TokenChars,TokenLen)}}.
<<'{L}+'>> : {token, {binary, TokenLine, token_to_binary(TokenChars, TokenLen)}}.

{L}+       : {token, {var, TokenLine, atom(TokenChars)}}.

[(),]      : {token, {atom(TokenChars), TokenLine}}.

{WS}+      : skip_token.

Erlang code.

atom(TokenChars) ->
    list_to_atom(string:to_lower(TokenChars)).

strip(TokenChars, TokenLen) ->
    lists:sublist(TokenChars, 2, TokenLen - 2).

token_to_binary(TokenChars, TokenLen) ->
    L = lists:sublist(TokenChars, 4, TokenLen - 6),
    list_to_binary(L).