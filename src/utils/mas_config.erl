%% @doc Defoult configurion and config-parsing functions.

-module(mas_config).
-include("mas.hrl").

-export([proplist_to_record/1]).

-define(LOAD(Prop, Proplist),
        Prop = proplists:get_value(Prop,Proplist)).

%% @doc Transform a proplist with config properties to a record
-spec proplist_to_record([tuple()]) -> config().
proplist_to_record(Proplist) ->
    #config{?LOAD(agent_env, Proplist),
            ?LOAD(topology, Proplist),
            ?LOAD(migration_probability, Proplist),
            ?LOAD(log_dir, Proplist),
            ?LOAD(islands, Proplist),
            ?LOAD(population_size, Proplist),
            ?LOAD(write_interval, Proplist),
            ?LOAD(skel_workers, Proplist),
            ?LOAD(arena_timeout, Proplist),
            ?LOAD(skel_split_size, Proplist),
            ?LOAD(skel_pull, Proplist)}.
