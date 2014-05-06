%% @author jstypka <jasieek@student.agh.edu.pl>
%% @version 1.1
%% @doc Model sekwencyjny, gdzie agenci sa na stale podzieleni w listach reprezentujacych wyspy.

-module(sequential_lists).
-export([start/4]).


-type agent() :: {Solution::genetic:solution(), Fitness::float(), Energy::pos_integer()}.
-type counter() :: dict().
-type island() :: [agent()].

%% ====================================================================
%% API functions
%% ====================================================================

-spec start(Time::pos_integer(), Islands::pos_integer(), Topology::topology:topology(), Path::string()) -> ok.
start(Time,Islands,Topology,Path) ->
    %%     io:format("{Model=sequential_lists,Time=~p,Islands=~p,Topology=~p}~n",[Time,Islands,Topology]),
    misc_util:seedRandom(),
    misc_util:clearInbox(),
    topology:start_link(self(),Islands,Topology),
    logger:start_link(Islands,Path),
    Environment = config:agent_env(),
    InitIslands = [Environment:initial_population() || _ <- lists:seq(1,Islands)],
    timer:send_after(Time,theEnd),
    timer:send_after(config:writeInterval(),write),
    {_Time,_Result} = timer:tc(fun loop/2, [InitIslands,misc_util:createNewCounter()]),
    topology:close(),
    logger:close().

%% ====================================================================
%% Internal functions
%% ====================================================================

%% @doc Glowa petla programu. Kazda iteracja powoduje ewolucje nowej generacji osobnikow.
-spec loop([island()],counter()) -> float().
loop(Islands,Counter) ->
    Environment = config:agent_env(),
    receive
        write ->
            logger:logLocalStats(sequential,
                                 fitness,
                                 [misc_util:result(I) || I <- Islands]),
            logger:logLocalStats(sequential,
                                 population,
                                 [length(I) || I <- Islands]),
            logger:logGlobalStats(sequential,Counter),
            timer:send_after(config:writeInterval(),write),
            loop(Islands,misc_util:createNewCounter());
        theEnd ->
            lists:max([misc_util:result(I) || I <- Islands])
    after 0 ->
            Groups = [misc_util:groupBy([{Environment:behaviour_function(Agent),Agent} || Agent <- I]) || I <- Islands],
            NewCounter = misc_util:countInteractions(Groups,Counter),
            Emigrants = [seq_migrate(lists:keyfind(migration,1,Island),Nr) || {Island,Nr} <- lists:zip(Groups,lists:seq(1,length(Groups)))],
            NewGroups = [[misc_util:meeting_proxy(Activity,sequential) || Activity <- I] || I <- Groups],
            WithEmigrants = append(lists:flatten(Emigrants),NewGroups),
            NewIslands = [misc_util:shuffle(lists:flatten(I)) || I <- WithEmigrants],
            loop(NewIslands,NewCounter)
    end.

-spec seq_migrate(false | {migration,[agent()]}, pos_integer()) -> [{migration,[agent()]}].
seq_migrate(false,_) ->
    [];
seq_migrate({migration,Agents},From) ->
    Destinations = [{topology:getDestination(From),Agent} || Agent <- Agents],
    misc_util:groupBy(Destinations).

-spec append({pos_integer(),[agent()]}, [list(agent())]) -> [list(agent())].
append([],Islands) ->
    Islands;
append([{Destination,Immigrants}|T],Islands) ->
    NewIslands = misc_util:mapIndex(Immigrants,Destination,Islands,fun lists:append/2),
    append(T,NewIslands).