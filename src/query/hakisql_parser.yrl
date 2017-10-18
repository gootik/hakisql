Nonterminals
    where_clause
    search_cond
    search_cond2
    search_cond3
    search_cond4
    predicate
    comparsion_pred
    in_pred
    atom_commalist
    scalar_exp
    column_ref
    atom
    literal.

Terminals
    op
    int
    float
    '(' ')'
    ','
    string
    'and'
    in
    'not'
    'or'
    var.

Rootsymbol where_clause.

where_clause -> search_cond : '$1'.

search_cond -> search_cond2 'or' search_cond : {'or', '$1', '$3'}.
search_cond -> search_cond2 : '$1'.

search_cond2 -> search_cond3 'and' search_cond2 : {'and', '$1', '$3'}.
search_cond2 -> search_cond3 : '$1'.

search_cond3 -> '(' search_cond ')' : '$2'.
search_cond3 -> search_cond4 : '$1'.

search_cond4 -> predicate : '$1'.

predicate -> comparsion_pred : '$1'.
predicate -> in_pred : '$1'.

comparsion_pred -> scalar_exp op scalar_exp : {op, value('$2'), '$1', '$3'}.

in_pred -> scalar_exp 'not' in '(' atom_commalist ')' : {notin, '$1', '$5'}.
in_pred -> scalar_exp in '(' atom_commalist ')' : {in, '$1', '$4'}.
in_pred -> scalar_exp 'not' in column_ref : {notin_var, '$1', '$4'}.
in_pred -> scalar_exp in column_ref : {in_var, '$1', '$3'}.


scalar_exp -> atom : '$1'.
scalar_exp -> column_ref : '$1'.
scalar_exp -> '(' scalar_exp ')' : '$2'.

atom -> literal : '$1'.

column_ref -> var : value('$1').

literal -> int : value('$1').
literal -> float : value('$1').
literal -> string : value('$1').

atom_commalist -> atom_commalist ',' atom : flatten(['$1', '$3']).
atom_commalist -> atom : ['$1'].

Erlang code.

flatten(List) -> lists:flatten(List).
value({_, _, Value}) -> Value.