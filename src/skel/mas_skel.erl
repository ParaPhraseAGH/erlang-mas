%% @author jstypka <jasieek@student.agh.edu.pl>
%% @version 1.1

-module(mas_skel).
-export([start/3,
         seed_random_once_per_process/0]).


-include ("mas.hrl").

-compile([{inline, [seed_random_once_per_process/0]}]).

-type sim_params() :: mas:sim_params().

%% ====================================================================
%% API functions
%% ====================================================================

-spec start(Time::pos_integer(), sim_params(), config()) -> ok.
start(Time, SP, Cf = #config{islands = Islands, agent_env = Env}) ->
    mas_topology:start_link(self(), Islands, Cf#config.topology),
    %%     mas_logger:start_link(lists:seq(1, Cf#config.islands), Cf),
    initialize_subscriptions(Cf),
    mas_misc_util:seed_random(),
    mas_misc_util:clear_inbox(),
    Population = [{I, Env:initial_agent(SP)} ||
                     _ <- lists:seq(1, Cf#config.population_size),
                     I <- lists:seq(1, Islands)],
    {_Time, Result} = timer:tc(fun main/4, [Population, Time, SP, Cf]),
    mas_topology:close(),
    %%     mas_logger:close(),
    Result.

%% ====================================================================
%% Internal functions
%% ====================================================================

%% @doc Main program loop
-spec main([{pos_integer(), mas:agent()}],
           non_neg_integer(), sim_params(), config()) -> [mas:agent()].
main(Population, Time, SP, Cf) ->
    EndTime = mas_misc_util:add_miliseconds(os:timestamp(), Time),
    Workers = Cf#config.skel_workers,

    TagFun = fun({Home, Agent}) ->
                     seed_random_once_per_process(),
                     {{Home, mas_misc_util:behaviour_proxy(Agent, SP, Cf)}, Agent}
             end,

    MigrateFun = fun({{Home, migration}, Agent}) ->
                         {{mas_topology:getDestination(Home), migration}, Agent};
                    (OtherAgent)->
                         OtherAgent
                 end,

    GroupFun = fun mas_misc_util:group_by/1,


    LogFun = fun(Groups) ->
                     log_countstats(Groups, Cf),
                     %%                      log_funstats(Groups, Cf),
                     Groups
             end,

    Split = fun(Groups) ->
                    lists:flatmap(
                      fun (Group) ->
                              split(Group, Cf#config.skel_split_size)
                      end,
                      Groups)
            end,

    TMGLS = fun (Agents) ->
                    Tagged = lists:map(TagFun,
                                       Agents),
                    Migrated = lists:map(MigrateFun,
                                         Tagged),
                    Grouped = GroupFun(Migrated),
                    LogFun(Grouped),
                    Split(Grouped)
            end,


    Work = {seq, fun({{Home, Behaviour}, Agents}) ->
                         seed_random_once_per_process(),
                         NewAgents =
                             mas_misc_util:meeting_proxy({Behaviour, Agents}, mas_skel, SP, Cf),
                         [{Home, A} || A <- NewAgents]
                 end },

    Shuffle = {seq, fun(Agents) ->
                            seed_random_once_per_process(),
                            mas_misc_util:shuffle(lists:flatten(Agents))
                    end},

    Map = case Cf#config.skel_pull of
              enable ->
                  {map, [Work], Workers, pull};
              _ ->
                  {map, [Work], Workers}
          end,

    Workflow = {pipe, [{seq, TMGLS},
                       Map,
                       Shuffle]},

    [FinalIslands] = skel:do([{feedback,
                               [Workflow],
                               _While = fun(_Agents) ->
                                                os:timestamp() < EndTime
                                        end}],
                             [Population]),
    _FinalAgents =
        [Agent || {_Island, Agent} <- FinalIslands].


%% ====================================================================
%% Internal functions
%% ====================================================================

-spec log_countstats([tuple()], config()) -> ok.
log_countstats(Groups, Cf) ->
    BigDict = dict:from_list([{I, mas_misc_util:create_new_counter(Cf)}
                              || I <- lists:seq(1, Cf#config.islands)]),

    NewBigDict = lists:foldl(fun({{Home, Behaviour}, Group}, AccBD) ->
                                     IslandDict = dict:fetch(Home, AccBD),
                                     NewIslandDict = dict:update_counter(Behaviour, length(Group), IslandDict),
                                     dict:store(Home, NewIslandDict, AccBD)
                             end, BigDict, Groups),

    %%     [[mas_logger:log_countstat(Island, Stat, Val)
    [[exometer:update([Island, Stat], Val)
      || {Stat, Val} <- dict:to_list(Counter)]
     || {Island, Counter} <- dict:to_list(NewBigDict)],

    ok.


-spec seed_random_once_per_process() -> ok.
seed_random_once_per_process() ->
    case get(was_seeded) of
        undefined ->
            mas_misc_util:seed_random(),
            put(was_seeded, true);
        true ->
            ok
    end.


%% @doc Split given group of agents into list of max size `Size`, with
%% keeping tags (island number in our case) exactly the same.x
-spec split(Group, Size) -> Groups when
      Groups :: [Group],
      Group :: {Tag, [mas:agent()]},
      Tag :: any(),
      Size :: pos_integer().

split(_Group = {Tag, Agents}, Size) ->
    AgentsLists = partition(Agents, Size),
    [{Tag, AL} || AL <- AgentsLists].


-spec partition([A], Size) -> [[A]] when
      A :: any(),
      Size :: pos_integer().
partition(List, Size) ->
    partition(List, Size, []).

partition(List, Size, Acc) when
      length (List) =< Size ->
    [List | Acc];
partition(List, Size, Acc) ->
    {Part, Rest} = lists:split(Size, List),
    partition(Rest, Size, [Part | Acc]).


-spec initialize_subscriptions(config()) -> [ok].
initialize_subscriptions(Cf = #config{islands = Islands,
                                      write_interval = Int}) ->
    Stats = mas_misc_util:determine_behaviours(Cf),
    [begin
         Metric = [I,S],
         exometer:new(Metric, counter),
         exometer_report:subscribe(mas_reporter, Metric, value, Int)
     end || S <- Stats, I <- lists:seq(1, Islands)].