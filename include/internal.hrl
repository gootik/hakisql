-include("types.hrl").

-define(INDEX_TABLE_POSTFIX, "_i").
-define(SCHEMA_TABLE_POSTFIX, "_schema").

-type internal_schema() :: #{
    cols => map(),
    num_rows => pos_integer(),
    table_name => table_name(),
    index_table => atom(),
    index_field_names => atom()}.

-export_type([
    internal_schema/0
]).
