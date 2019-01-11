Definitions.

D   = [0-9]
L   = [A-Za-z0-9][A-Za-z0-9_]*
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
{D}+\.{D}+ : {token, {float, TokenLine, list_to_float(TokenChars)}}.

'{L}+'     : {token, {string, TokenLine, strip(TokenChars,TokenLen)}}.
<<'{L}+'>> : {token, {binary, TokenLine, token_to_binary(TokenChars, TokenLen)}}.

{L}+       : chars(TokenLine, TokenChars).

{.*}       : {token, {tuple, TokenLine, token_to_tuple(TokenChars, TokenLen)}}.

[(),]      : {token, {atom(TokenChars), TokenLine}}.

{WS}+      : skip_token.

Erlang code.

is_query_word('and') -> true;
is_query_word('in') -> true;
is_query_word('has') -> true;
is_query_word('or') -> true;
is_query_word('not') -> true;
is_query_word(_) -> false.

chars(TokenLine, TokenChars) ->
    Word = atom(TokenChars),
    case is_query_word(Word) of
        true -> {token, {Word, TokenLine, Word}};
        false -> {token, {var, TokenLine, Word}}
    end.

atom(TokenChars) ->
    list_to_atom(string:to_lower(TokenChars)).

strip(TokenChars, TokenLen) ->
    lists:sublist(TokenChars, 2, TokenLen - 2).

token_to_binary(TokenChars, TokenLen) ->
    L = lists:sublist(TokenChars, 4, TokenLen - 6),
    list_to_binary(L).

%% OMG HAX
token_to_tuple(TokenChars, _TokenLen) ->
    {ok, L, _} = erl_scan:string(TokenChars ++ "."),
    {ok, T} = erl_parse:parse_term(L),
    T.