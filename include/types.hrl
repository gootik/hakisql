-type table_name() :: atom().
-type table_column_definition() :: #{column_name() => [column_property()]}.
-type table_row() :: map().

-type column_name() :: atom().
-type column_property() :: col_indexing_options() | col_types().

-type col_indexing_options() :: index.
-type col_types() :: string | number | atom.

-type select_query() :: string().

-export_type([
    table_name/0,
    table_column_definition/0,
    table_row/0,

    select_query/0
]).