-ifndef(_TYPES_H_).
-define(_TYPES_H_, true).

-type table_name() :: atom().
-type table_column_definition() :: #{column_name() => [column_property()]}.

-type column_name() :: atom().
-type column_property() :: col_indexing_options() | col_types().
-type column_value() :: list() | string() | atom() | number() | tuple() | bitstring().

-type col_indexing_options() :: index.
-type col_types() :: string | number | atom.

-type table_row() :: #{column_name() => column_value()}.

-type select_query() :: string().

-export_type([
    table_name/0,
    table_column_definition/0,
    table_row/0,

    select_query/0
]).

-endif.