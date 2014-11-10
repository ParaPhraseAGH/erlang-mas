%% @doc Default configurion and config-parsing functions.

-module(mas_config).
-include("mas.hrl").

-export([proplist_to_record/1]).

-define(LOAD(Proplist, Prop),
        Prop = case proplists:lookup(Prop, Proplist) of
                   {Prop, Value} ->
                       Value;
                   none ->
                       erlang:error({"mas missing option", Prop})
               end).

-define(LOAD(Proplist, Prop, Default),
        Prop = proplists:get_value(Prop, Proplist, Default)).

%% @doc Transform a proplist with config properties to a record
-spec proplist_to_record([tuple()]) -> config().
proplist_to_record(Proplist) ->
    #config{?LOAD(Proplist, agent_env),
            ?LOAD(Proplist, topology, mesh),
            ?LOAD(Proplist, migration_probability, 0.0001),
            ?LOAD(Proplist, log_dir, standard_io),
            ?LOAD(Proplist, islands, 4),
            ?LOAD(Proplist, population_size, 100),
            ?LOAD(Proplist, write_interval, 1000),
            ?LOAD(Proplist, skel_workers, 4),
            ?LOAD(Proplist, arena_timeout, 5000),
            ?LOAD(Proplist, skel_split_size, 20),
            ?LOAD(Proplist, skel_pull, enable)}.
