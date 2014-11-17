%% @doc This module starts the mas framework with given environment,
%% time and parameters

-module (mas).
-export ([start/3]).
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


-spec start(pos_integer(), sim_params(), [tuple()]) -> [agent()].
start(Time, SP, Options) ->
    ConfigRecord = mas_config:proplist_to_record(Options),
    io:format("### ConfigRecord: ~w~n", [ConfigRecord]),
    Model = ConfigRecord#config.model,
    Model:start(Time, SP, ConfigRecord).
