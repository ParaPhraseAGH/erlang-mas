%% @doc Defoult configurion and config-parsing functions.

-module(mas_config).
-include("mas.hrl").

-export([proplist_to_record/1]).

-define(LOAD(Prop, Dict), Prop = dict:fetch(Prop,Dict)).

%% @doc Transform a proplist with config properties to a record
-spec proplist_to_record([tuple()]) -> config().
proplist_to_record(Proplist) ->
    Dict = dict:from_list(Proplist),
    #config{?LOAD(agent_env, Dict),
            ?LOAD(topology, Dict),
            ?LOAD(migration_probability, Dict),
            ?LOAD(log_dir, Dict),
            ?LOAD(islands, Dict),
            ?LOAD(population_size, Dict),
            ?LOAD(write_interval, Dict),
            ?LOAD(skel_workers, Dict),
            ?LOAD(arena_timeout, Dict),
            ?LOAD(skel_split_size, Dict),
            ?LOAD(skel_pull, Dict)}.
