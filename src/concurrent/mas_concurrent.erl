%% @author jstypka <jasieek@student.agh.edu.pl>
%% @version 1.0
%% @doc This is the main module of concurrent model. It handles starting the system and cleaning after work

-module(mas_concurrent).
-export([start/3, send_result/1]).
-include("mas.hrl").

-define(RESULT_SINK, result_sink).
-define(RESULT_SINK_TIMEOUT, 2500).

-type agent() :: mas:agent().

%% ====================================================================
%% API functions
%% ====================================================================

-spec start(Time::pos_integer(), mas:sim_params(), config()) -> [agent()].
start(Time, SP, Cf = #config{islands = Islands}) ->
    mas_misc_util:clear_inbox(),
    mas_topology:start_link(self(), Islands, Cf#config.topology),
    Supervisors = [mas_conc_supervisor:start(SP, Cf) || _ <- lists:seq(1,Islands)],
    mas_misc_util:initialize_subscriptions(Supervisors, Cf),
    receive
        ready ->
            trigger(Supervisors)
    end,
    register(?RESULT_SINK, self()),
    timer:sleep(Time),
    [ok = mas_conc_supervisor:close(Pid) || Pid <- Supervisors],
    mas_topology:close(),
    Agents = receive_results(),
    unregister(?RESULT_SINK),
    Agents.

-spec send_result(agent()) -> ok.
send_result(Agent) ->
    whereis(?RESULT_SINK) ! {result, Agent},
    ok.

%% ====================================================================
%% Internal functions
%% ====================================================================

-spec trigger([pid()]) -> [ok].
trigger(Supervisors) ->
    [mas_conc_supervisor:go(Pid) || Pid <- Supervisors].

-spec receive_results() -> [agent()].
receive_results() ->
    receive_results([]).

-spec receive_results([agent()]) -> [agent()].
receive_results(Acc) ->
    receive
        {result, Agent} ->
            receive_results([Agent | Acc])
    after ?RESULT_SINK_TIMEOUT ->
            Acc
    end.
