-type table_name() :: atom().
-type table_column_definition() :: #{column_name() => [column_property()]}.

-type column_name() :: atom().
-type column_property() :: col_indexing_options() | col_types().

-type col_indexing_options() :: index.
-type col_types() :: string | number | atom.

-export_type([
    table_column_definition/0,
    table_name/0
]).