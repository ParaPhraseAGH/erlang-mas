%% @author jstypka <jasieek@student.agh.edu.pl>
%% @version 1.1
%% @doc This is the main module of sequential model. It handles starting the system and cleaning after work

-module(mas_sequential).
-export([start/3]).

-include ("mas.hrl").

-type agent() :: mas:agent().
-type sim_params() :: mas:sim_params().

%% ====================================================================
%% API functions
%% ====================================================================

-spec start(Time::pos_integer(), sim_params(), config()) -> [agent()].
start(Time, SP, Cf = #config{islands = Islands}) ->
    mas_misc_util:seed_random(),
    mas_misc_util:clear_inbox(),
    mas_misc_util:initialize_subscriptions(lists:seq(1, Islands), Cf),
    mas_topology:start_link(self(), Islands, Cf#config.topology),
    InitIslands = [mas_misc_util:generate_population(SP, Cf)
                   || _ <- lists:seq(1, Islands)],
    timer:send_after(Time, the_end),
    {ok, TRef} = timer:send_interval(Cf#config.write_interval, write),
    {_Time, Result} = timer:tc(fun loop/4,
                               [InitIslands,
                                [mas_misc_util:create_new_counter(Cf)
                                 || _ <- lists:seq(1, Islands)],
                                SP,
                                Cf]),
    timer:cancel(TRef),
    mas_misc_util:close_subscriptions(lists:seq(1, Islands), Cf),
    mas_topology:close(),
    Result.

%% ====================================================================
%% Internal functions
%% ====================================================================

%% @doc The main island process loop.
%% A new generation of the population is created in every iteration.
-spec loop([island()], [counter()], sim_params(), config()) -> [agent()].
loop(Islands, Counters, SP, Cf) ->
    receive
        write ->
            [log_island(Nr, C)
             || {Nr, C} <- lists:zip(lists:seq(1, length(Islands)),
                                     Counters)],

            loop(Islands,
                 [mas_misc_util:create_new_counter(Cf)
                  || _ <- lists:seq(1, length(Islands))],
                 SP,
                 Cf);
        the_end ->
            lists:flatten(Islands)
    after 0 ->
            Tag = fun(Island) ->
                          [{mas_misc_util:behaviour_proxy(Agent,
                                                          SP,
                                                          Cf), Agent}
                           || Agent <- Island]
                  end,

            Groups = [mas_misc_util:group_by(Tag(I))
                      || I <- Islands],

            IslandSeq = lists:seq(1, length(Groups)),

            Emigrants = [seq_migrate(lists:keyfind(migration, 1, Island), Nr)
                         || {Island, Nr} <- lists:zip(Groups, IslandSeq)],

            NewGroups = [[mas_misc_util:meeting_proxy(Activity,
                                                      mas_sequential,
                                                      SP,
                                                      Cf)
                          || Activity <- I]
                         || I <- Groups],

            WithEmigrants = append(lists:flatten(Emigrants), NewGroups),

            NewIslands = [mas_misc_util:shuffle(lists:flatten(I))
                          || I <- WithEmigrants],

            NewCounters = [mas_misc_util:add_interactions_to_counter(G, C)
                           || {G, C} <- lists:zip(Groups, Counters)],

            loop(NewIslands, NewCounters, SP, Cf)
    end.

-spec log_island(pos_integer(), counter()) -> [ok].
log_island(Key, Counter) ->
    [exometer:update([Key, Interaction], Val)
     || {Interaction, Val} <- dict:to_list(Counter)].


-spec seq_migrate(false | {migration, [agent()]}, pos_integer()) ->
                         [{migration, [agent()]}].
seq_migrate(false, _) ->
    [];

seq_migrate({migration, Agents}, From) ->
    Destinations = [{mas_topology:getDestination(From), Agent}
                    || Agent <- Agents],
    mas_misc_util:group_by(Destinations).


-spec append([{pos_integer(), [agent()]}], [list(agent())]) -> [list(agent())].
append([], Islands) ->
    Islands;

append([{Destination, Immigrants} | T], Islands) ->
    NewIslands = mas_misc_util:map_index(Immigrants,
                                         Destination,
                                         Islands,
                                         fun lists:append/2),
    append(T, NewIslands).
