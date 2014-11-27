%% @author jstypka <jasieek@student.agh.edu.pl>
%% @version 1.0
%% @doc This module handles the logic of a single island in hybrid model

-module(mas_hybrid_island).
-export([start/2, close/1, sendAgent/2]).

-include ("mas.hrl").

-type agent() :: mas:agent().
-type sim_params() :: mas:sim_params().

%% ====================================================================
%% API functions
%% ====================================================================
%% @doc Generates initial data and starts the computation
-spec start(sim_params(), config()) -> no_return().
start(SP, Cf) ->
    mas_misc_util:seed_random(),
    Agents = mas_misc_util:generate_population(SP, Cf),
    timer:send_interval(Cf#config.write_interval, write),

    Result = loop(Agents,
                  mas_misc_util:create_new_counter(Cf),
                  SP,
                  Cf),

    mas_hybrid:send_result(Result).

-spec close(pid()) -> {finish, pid()}.
close(Pid) ->
    Pid ! {finish, self()}.


%% @doc Asynchronusly sends an agent immigrating to this island
-spec sendAgent(pid(), agent()) -> {agent, pid(), agent()}.
sendAgent(Pid, Agent) ->
    Pid ! {agent, self(), Agent}.

%% ====================================================================
%% Internal functions
%% ====================================================================
%% @doc The main island process loop.
%% A new generation of the population is created in every iteration.
-spec loop([agent()], counter(), sim_params(), config()) -> [agent()].
loop(Agents, InteractionCounter, SP, Cf) ->
    receive
        write ->
            [exometer:update([self(), Interaction], Val)
             || {Interaction, Val} <- dict:to_list(InteractionCounter)],

            loop(Agents,
                 mas_misc_util:create_new_counter(Cf),
                 SP,
                 Cf);

        {agent, _Pid, A} ->
            loop([A | Agents], InteractionCounter, SP, Cf);

        {finish, _Pid} ->
            Agents
    after 0 ->
            Tagged = [{mas_misc_util:behaviour_proxy(A, SP, Cf), A}
                      || A <- Agents ],

            Groups = mas_misc_util:group_by(Tagged),

            NewGroups = [mas_misc_util:meeting_proxy(G, mas_hybrid, SP, Cf)
                         || G <- Groups],

            NewAgents = mas_misc_util:shuffle(lists:flatten(NewGroups)),

            NewCounter =
                mas_misc_util:add_interactions_to_counter(Groups,
                                                          InteractionCounter),

            loop(NewAgents, NewCounter, SP, Cf)
    end.
