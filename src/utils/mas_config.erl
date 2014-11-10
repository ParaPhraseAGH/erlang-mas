%% @doc Default configurion and config-parsing functions.

-module(mas_config).
-include("mas.hrl").

-export([proplist_to_record/1,
         options_specs/0]).

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
proplist_to_record(Options) ->
    Proplist = Options ++ default_options(),
    #config{?LOAD(Proplist, agent_env),
            ?LOAD(Proplist, topology),
            ?LOAD(Proplist, migration_probability),
            ?LOAD(Proplist, log_dir),
            ?LOAD(Proplist, islands),
            ?LOAD(Proplist, population_size),
            ?LOAD(Proplist, write_interval),
            ?LOAD(Proplist, arena_timeout),
            ?LOAD(Proplist, skel_workers),
            ?LOAD(Proplist, skel_split_size),
            ?LOAD(Proplist, skel_pull)}.


-spec options_specs() -> [getopt:option_spec()].
options_specs() ->
    [{agent_env,              $E,         "agent_env",              atom,
     "Module implementing `mas_agent_env` behaviour"},

     {topology,               $T,        "topology",                {atom, mesh},
      "Island topologies (ring, mes)h"},

     {migration_probability,  undefined, "migration_probability",   {float, 0.0001},
      "The probability of migration of an agent with positive energy"},

     {log_dir,                $L,         "log_dir",                {atom, standard_io},
     "The default path to write the logs to. The `standard_io` atom cause the logs to be sent to the standard output"},

     {islands,                $I,         "islands",                {integer, 4},
      "The number of islands"},

     {population_size,        $P,         "population_size",        {integer, 100},
      "The initial size of an island's population"},

     {write_interval,         $W,         "write_interval",         {integer, 1000},
      "How often the logs are writen to output (in milliseconds)"},

     {arena_timeout,          undefined,  "arena_timeout",          {integer, 5000},
      "How long an arena should wait for agents to come before raising an error (in milliseconds or the atom infinity)"},

     {skel_workers,           undefined,  "skel_workers",           {integer, 4},
      "Number of workers used in skel map skeleton."},

     {skel_split_size,        undefined,  "skel_split_size",        {integer, 20},
      "Granularity of workload send to each worker.  To disable set to value greater that `population_size`"},

     {skel_pull,              undefined,  "skel_pull",              {atom, enable},
      "`enable` or `disable` work-pulling in skel map."}
    ].


default_options() ->
    {ok, {Options, [] = _Other}} =
        getopt:parse(options_specs(), ""),
    Options.
