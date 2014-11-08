%% @doc This module starts the mas framework with given environment, model and parameters

-module (mas).
-export ([start/5]).
-export_type([agent/0,
              agent/1,
              sim_params/0,
              sim_params/1,
              agent_behaviour/0,
              agent_behaviour/1]).

-include ("mas.hrl").

-type agent(Any) :: Any.
-type agent() :: agent(any()).

-type sim_params(Any) :: Any.
-type sim_params() :: sim_params(any()).

-type agent_behaviour(Any) :: Any.
-type agent_behaviour() :: agent_behaviour(any()).


-spec start(atom(), model(), pos_integer(), sim_params(), [tuple()]) -> [agent()].
start(Module, Model, Time, SP, Options) ->
    ConfigFile = filename:join(mas_misc_util:get_config_dir(), "mas.config"),
    {ok, ConfigFromFile} = file:consult(ConfigFile),
    ConfigWithEnv = [{agent_env,Module}|ConfigFromFile],
    OverwrittenConfig = mas_misc_util:overwrite_options(Options, ConfigWithEnv),
    ConfigRecord = mas_config:proplist_to_record(OverwrittenConfig),
    Model:start(Time, SP, ConfigRecord).


