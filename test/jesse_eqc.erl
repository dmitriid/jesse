%%%-------------------------------------------------------------------
%%% @doc Quickcheck tests for jesse
%%%
%%% @author Dmitrii Dimandt
%%%-------------------------------------------------------------------
-module(jesse_eqc).

%%_* Exports ===================================================================
-compile(export_all).
-export([]).

%%_* Includes ==================================================================
-include_lib("eqc/include/eqc.hrl").

%%_* Defines ===================================================================

%% http://json-schema.org/latest/json-schema-core.html#anchor8
-define(types, [array, boolean, integer, number, null, object, string, any]).

%%_* Properties ================================================================
%%
%% @doc Property corresponds to "JSON-Schema-Test-Suite"/type.json except for
%%      the following:
%%        - when types includes a schema it should fully validate the schema
%%
%%      Given a list of {Type, Value}, for each of these:
%%          Generate schemas that may or may not validate this correctly
%%          For each schema:
%%                    Check to see if it validates the {Type, Value} passed in
%%            andalso Check that jesse validation returns the same result
%%
prop_type() ->
  ?FORALL( Data
         , generate_data()
         , ?FORALL( {Schema, DataToValidate}
                  , {generate_schemas(Data), oneof(Data)}
                  , begin
                      {_, Value} = DataToValidate,
                      Types = get_schema_types(Schema),
                      equals( validate_types(Types, DataToValidate, Schema)
                            , jesse:validate_with_schema(Schema, Value)
                            )
                    end)
         ).

%%_* Generators ================================================================
generate_data() ->
  ?LET( Type
      , non_empty(list(elements(?types)))
      , generate_data(Type)
      ).

generate_data(Type) when is_atom(Type) ->
  {Type, jesse_gen:gen(Type)};
generate_data(Types) when is_list(Types) ->
  [{T, jesse_gen:gen(T)} || T <- Types].

%% We generate jiffy/eep format
%% TODO: generate other formats as well
generate_schemas({_, _} = Type) ->
  {[{ <<"type">>
    , oneof([generate_partial_schema(Type)
            ])
    }]};
generate_schemas(L) when is_list(L) ->
  ?LET( Types
      , non_empty(list(elements(L)))
      , ?LET( GeneratedTypes
            , oneof([ generate_partial_schema(L)
                    , generate_partial_schema(Types)
                    ])
            , {[{<<"type">>, GeneratedTypes}]}
            )
      ).

generate_partial_schema({Type, _}) ->
  oneof([ generate_simple_schema_type(Type)
        , generate_partial_schema_type(Type)
        ]);
generate_partial_schema([_|_] = L) ->
  [generate_partial_schema(T) || T <- L].

generate_simple_schema_type(Type) ->
  atom_to_binary(Type, latin1).

generate_partial_schema_type(Type) ->
  {[{<<"type">>, atom_to_binary(Type, latin1)}]}.

get_schema_types({[{<<"type">>, Types}]}) ->
  get_schema_types(Types);
get_schema_types(L) when is_list(L) ->
  [get_schema_types(E) || E <- L];
get_schema_types(B) when is_binary(B) ->
  binary_to_atom(B, latin1).

validate_types(Types, {Type, Value}, Schema) ->
  case lists:any(fun(T) -> validate_type(T, {Type, Value}) end, Types) of
    true  -> {ok, Value};
    false -> {error
             , [{data_invalid
                , Schema
                , wrong_type
                , Value
                , []
                }
               ]
             }
  end.

validate_type(Type, {Type, _}) ->
  true;
validate_type(any, _) ->
  true;
validate_type(number, {Type, _}) ->
  Type == integer orelse Type == number;
validate_type(integer, {_, Value}) ->
  is_integer(Value);
validate_type(object, {_, Value}) ->
  case Value of
    {[_|_]} -> true;
    _       -> false
  end;
validate_type(array, {_, Value}) ->
  is_list(Value);
validate_type(_T1, _T2) ->
  false.
