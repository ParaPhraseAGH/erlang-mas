%% @author jstypka <jasieek@student.agh.edu.pl>
%% @version 1.1
-module(mas_skel).
-export([start/3]).
-include ("mas.hrl").

-type sim_params() :: mas:sim_params().
%% ====================================================================
%% API functions
%% ====================================================================
-spec start(Time::pos_integer(), sim_params(), config()) -> [mas:agent()].
start(Time, SP, Cf = #config{islands = Islands, agent_env = Env}) ->
    mas_topology:start_link(self(), Islands, Cf#config.topology),
    mas_misc_util:seed_random(),
    mas_misc_util:clear_inbox(),
    ets:new(migration_ets, [duplicate_bag,
                            public,
                            named_table,
                            {read_concurrency, true}]),
    Population = [{I, [ Env:initial_agent(SP)
                        || _ <- lists:seq(1, Cf#config.population_size)]}
                  || I <- lists:seq(1, Islands)],
    mas_misc_util:initialize_subscriptions(lists:seq(1, Islands), Cf),
    {_Time, Result} = timer:tc(fun main/4, [Population, Time, SP, Cf]),
    mas_misc_util:close_subscriptions(lists:seq(1, Islands), Cf),
    mas_topology:close(),
    ets:delete(migration_ets),
    Result.

%% ====================================================================
%% Internal functions
%% ====================================================================

%% @doc Main program loop
-spec main([{pos_integer(), [mas:agent()]}],
           non_neg_integer(), sim_params(), config()) -> [mas:agent()].
main(Population, Time, SP, Cf) ->
    EndTime = mas_misc_util:add_miliseconds(os:timestamp(), Time),
    Workers = Cf#config.skel_workers,

    Tag = fun({IsNo, Island}) ->
                  mas_skel_old:seed_random_once_per_process(),
                  Tagged = [{mas_misc_util:behaviour_proxy(Agent, SP, Cf),
                             Agent}
                            || Agent <- Island],
                  {IsNo, Tagged}
          end,

    Group = fun ({IsNo, Island}) ->
                    {IsNo, mas_misc_util:group_by(Island)}
            end,

    Log = fun({IsNo, Island}) ->
                  [exometer:update([IsNo, Stat], length(Agents))
                   || {Stat, Agents} <- Island],
                  {IsNo, Island}
          end,


    Migrate = fun({IsNo, Island}) ->
                      Emigrants = case lists:keyfind(migration, 1, Island) of
                                      false ->
                                          [];
                                      {migration, Agents} ->
                                          Agents
                                  end,

                      Destinated = [{mas_topology:getDestination(A), A}
                                    || A <- Emigrants],

                      Leftovers = insert_emigrants(Destinated),

                      Immigrants = ets:lookup(migration_ets, IsNo),

                      [true = ets:delete_object(migration_ets, Imm)
                       || Imm <- Immigrants],

                      Im = [A || {_Island, A} <- Immigrants],

                      NewIslands = lists:keydelete(migration, 1, Island),

                      {IsNo, [{migration, Leftovers ++ Im} | NewIslands]}
              end,

    Work = fun({IsNo, Island}) ->
                   NA = [mas_misc_util:meeting_proxy({Behaviour, Agents},
                                                     mas_skel,
                                                     SP,
                                                     Cf)
                         || {Behaviour, Agents} <- Island],
                   {IsNo, lists:flatten(NA)}
           end,

    Shuffle = fun({IsNo, Island}) ->
                      {IsNo, mas_misc_util:shuffle(Island)}
              end,

    Pipe = {pipe, [{seq, Fun} || Fun <- [Tag,
                                         Group,
                                         Log,
                                         Migrate,
                                         Work,
                                         Shuffle]]},

    FinalIslands = skel:do([{map, [{feedback,
                                    [Pipe],
                                    _While = fun(_Islands) ->
                                                     os:timestamp() < EndTime
                                             end}], Workers, pull}],
                           [Population]),


    AddImmigrants = ets:tab2list(migration_ets) ++ lists:flatten(FinalIslands),
    lists:flatten([Agents || {_Island, Agents} <- AddImmigrants]).

%% ====================================================================
%% Internal functions
%% ====================================================================

-spec insert_emigrants([{pos_integer(), mas:agent()}]) -> [mas:agent()].
insert_emigrants(Emigrants) ->
    case ets:insert_new(migration_ets, Emigrants) of
        true ->
            [];
        false ->
            Res = [ets:insert_new(migration_ets, E) || E <- Emigrants],
            [E || {false, {_Home, E}} <- lists:zip(Res, Emigrants)]
    end.