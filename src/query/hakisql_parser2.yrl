Nonterminals
    predicates
    predicate
    list
    element
    elements.

Terminals
    '(' ')'
    ','
    atom
    var
    integer
    string
    'in'
    'or'
    'and'
    'op'
    'has'.

Rootsymbol predicates.

predicates -> predicate : '$1'.
predicates -> predicate 'or' predicate : {'or', '$1', '$3'}.
predicates -> predicates 'or' predicate : {'or', '$1', '$3'}.

predicates -> predicate 'and' predicate : {'and', '$1', '$3'}.

predicate -> var 'in' list : {'op', 'in', unwrap('$1'), '$3'}.
predicate -> var 'has' element : {'op', 'has', unwrap('$1'), '$3'}.

predicate -> var 'op' element : {'op', unwrap('$2'), unwrap('$1'), '$3'}.

list -> '(' ')' : nil.
list -> '(' elements ')' : {list,'$2'}.

elements -> element : ['$1'].
elements -> element ',' elements : ['$1'] ++ '$3'.

element -> atom : '$1'.
element -> var : unwrap('$1').
element -> integer : unwrap('$1').
element -> float : unwrap('$1').
element -> string : unwrap('$1').

Erlang code.

unwrap({_,_,V}) -> V.