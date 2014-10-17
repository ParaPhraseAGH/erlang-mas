%% @author jstypka <jasieek@student.agh.edu.pl>
%% @version 1.0
%% @doc This module handles current islands topology info and also computes destination for migrating agents

-module(mas_topology).
-behaviour(gen_server).

%% API
-export([start_link/3, helloPort/0, emigrant/1, getDestination/1, close/0]).
%% gen_server
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
         code_change/3]).

-export_type([topology/0]).

-type topology() :: mesh | ring.

%% ====================================================================
%% API functions
%% ====================================================================
-spec start_link(pid(),integer(),topology()) -> {ok,pid()}.
start_link(King, IslandsNr, Topology) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [King, IslandsNr, Topology], []).

-spec helloPort() -> ok.
helloPort() ->
    gen_server:cast(whereis(?MODULE),{helloPort,self()}).

-spec emigrant({tuple(),tuple()}) -> ok.
emigrant(AgentInfo) ->
    gen_server:cast(whereis(?MODULE),{emigrant,self(),AgentInfo}).

-spec close() -> ok.
close() ->
    gen_server:cast(whereis(?MODULE),close).


%% @doc Synchronously computes a new destination based on a given parameter
-spec getDestination(integer()) -> integer().
getDestination(X) ->
    gen_server:call(whereis(?MODULE),{destination,X},infinity).

%% ====================================================================
%% Callbacks
%% ====================================================================
-record(state, {n = 0 :: integer(),
                king :: pid(),
                topology = undefined :: atom(),
                ports = [] :: [pid()]}).

-type state() :: #state{}.

-spec init(list()) -> {ok,state()}.
init([King, IslandsNr, Topology]) ->
    mas_misc_util:seed_random(),
    {ok, #state{n = IslandsNr, topology = Topology, king = King}}.

-spec handle_call(term(),{pid(),term()},state()) -> {reply,term(),state()} |
                                                    {reply,term(),state(),hibernate | infinity | non_neg_integer()} |
                                                    {noreply,state()} |
                                                    {noreply,state(),hibernate | infinity | non_neg_integer()} |
                                                    {stop,term(),term(),state()} |
                                                    {stop,term(),state()}.
handle_call({destination, X}, _From, State) ->
    %%     io:format("Emigration from ~p to ~p~n",[X,computeDestination(X,State)]),
    {reply, computeDestination(X,State), State}.

-spec handle_cast(term(),state()) -> {noreply,state()} |
                                     {noreply,state(),hibernate | infinity | non_neg_integer()} |
                                     {stop,term(),state()}.
handle_cast({emigrant,Pid,AgentInfo}, St) ->
    NewPort = case St#state.n of
                  1 -> Pid;
                  _ ->
                      OldPort = mas_misc_util:find(Pid,St#state.ports),
                      %%                       io:format("Emigration from ~p to ~p~n",[OldPort,computeDestination(OldPort,St)]),
                      lists:nth(computeDestination(OldPort, St), St#state.ports)
              end,
    mas_conc_port:immigrate(NewPort,AgentInfo),
    {noreply, St};

handle_cast({helloPort,Pid}, St) ->
    N = St#state.n,
    case length(St#state.ports) + 1 of
        N ->
            St#state.king ! ready;
        _ ->
            nothing
    end,
    {noreply, St#state{ports = [Pid|St#state.ports]}};

handle_cast(close, State) ->
    {stop, normal, State}.

-spec handle_info(term(),state()) -> {noreply,state()} |
                                     {noreply,state(),hibernate | infinity | non_neg_integer()} |
                                     {stop,term(),state()}.
handle_info(_Request, State) ->
    {noreply, State}.

-spec terminate(term(),state()) -> no_return().
terminate(_Reason, _State) ->
    ok.

-spec code_change(term(),state(),term()) -> {ok, state()}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ====================================================================
%% Internal functions
%% ====================================================================

computeDestination(From, St) ->
    case St#state.n of
        1 -> 1;
        N ->
            case St#state.topology of
                ring ->
                    NewIsland = case random:uniform() < 0.5 of
                                    true -> From + 1;
                                    false -> From - 1
                                end,
                    case NewIsland - 1 of
                        -1 -> N;
                        N -> 1;
                        _ -> NewIsland
                    end;
                mesh ->
                    Destinations = [I || I <- lists:seq(1, N), I =/= From],
                    Index = random:uniform(length(Destinations)),
                    lists:nth(Index, Destinations);
                _ ->
                    erlang:error(wrongTopology)
            end
    end.
