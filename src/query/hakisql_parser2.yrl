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
    binary
    float
    tuple
    'not'
    'in'
    'or'
    'and'
    'op'
    'has'.

Rootsymbol predicates.

predicates -> predicate : '$1'.

predicate -> predicate 'or' predicate : {'or', '$1', '$3'}.

predicate -> predicate 'and' predicate : {'and', '$1', '$3'}.

predicate -> var 'in' list : {'op', 'in', unwrap('$1'), '$3'}.
predicate -> var 'not' 'in' list : {'op', 'notin', unwrap('$1'), '$4'}.

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
element -> binary : unwrap('$1').
element -> tuple : unwrap('$1').

Erlang code.

unwrap({_,_,V}) -> V.