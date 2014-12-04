%% @author jstypka <jasieek@student.agh.edu.pl>
%% @version 1.1
%% @doc This is the main module of hybrid model. It handles starting the system, migrating agents and cleaning after work

-module(mas_hybrid).
-behaviour(gen_server).

%% API
-export([start/3,  sendAgent/1]).
%% gen_server
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
         code_change/3, send_result/1]).

-include ("mas.hrl").

-define(RESULT_SINK, result_sink).
-type agent() :: mas:agent().
-type sim_params() :: mas:sim_params().

-record(state, {pids :: list(pid()),
                config :: config()}).
-type state() :: #state{}.

%% ====================================================================
%% API functions
%% ====================================================================
-spec start(Time::pos_integer(), sim_params(), config()) -> [agent()].
start(Time, SP, Cf) ->
    {ok, _} = gen_server:start({local, ?MODULE}, ?MODULE, [Time, SP, Cf], []),
    register(?RESULT_SINK, self()),
    Islands = [receive_results() || _ <- lists:seq(1, Cf#config.islands)],
    unregister(?RESULT_SINK),
    lists:flatten(Islands).

%% @doc Asynchronously sends an agent from an arena to the supervisor
-spec sendAgent(agent()) -> ok.
sendAgent(Agent) ->
    gen_server:cast(whereis(?MODULE), {agent, self(), Agent}).

%% @doc Asynchronously send back result from an island
-spec send_result([agent()]) -> ok.
send_result(Agents) ->
    whereis(?RESULT_SINK) ! {result, Agents},
    ok.

%% ====================================================================
%% Callbacks
%% ====================================================================
-spec init(Args :: term()) -> {ok, State :: state()} |
                              {ok, State :: state(), timeout() | hibernate} |
                              {stop, Reason :: term()} | ignore.
init([Time, SP, Cf = #config{islands = Islands}]) ->
    timer:send_after(Time, theEnd),
    Pids = [spawn_link(mas_hybrid_island, start, [SP, Cf])
            || _ <- lists:seq(1, Islands)],
    mas_topology:start_link(self(), Islands, Cf#config.topology),
    mas_misc_util:initialize_subscriptions(Pids, Cf),
    {ok, #state{pids = Pids,
                config = Cf}}.

-spec handle_call(Request :: term(),
                  From :: {pid(), Tag :: term()},
                  State :: state()) ->
                         {reply, Reply :: term(), NewState :: state()} |
                         {reply, Reply :: term(), NewState :: state(),
                          timeout() | hibernate} |
                         {noreply, NewState :: state()} |
                         {noreply, NewState :: state(),
                          timeout() | hibernate} |
                         {stop, Reason :: term(), Reply :: term(),
                          NewState :: state()} |
                         {stop, Reason :: term(), NewState :: state()}.
handle_call(_, _, State) ->
    {noreply, State}.

-spec handle_cast(Request :: term(), State :: state())
                 -> {noreply, NewState :: state()} |
                    {noreply, cleaning, timeout() | hibernate} |
                    {stop, Reason :: term(), NewState :: state()}.
handle_cast({agent, From, Agent}, St = #state{pids = Pids}) ->
    IslandFrom = mas_misc_util:find(From, Pids),
    IslandTo = mas_topology:getDestination(IslandFrom),
    mas_hybrid_island:sendAgent(lists:nth(IslandTo, Pids), Agent),
    {noreply, St}.

-spec handle_info(Info :: timeout() | term(), State :: state())
                 -> {noreply, NewState :: state()} |
                    {noreply, NewState :: state(), timeout() | hibernate} |
                    {stop, Reason :: term(), NewState :: state()}.
handle_info(theEnd, State) ->
    {stop, normal, State}.

-spec terminate(Reason :: (normal | shutdown | {shutdown, term()} | term()),
                State :: state()) -> term().
terminate(_Reason, #state{pids = Pids, config = Cf}) ->
    [mas_hybrid_island:close(Pid) || Pid <- Pids],
    mas_misc_util:close_subscriptions(Pids, Cf),
    mas_topology:close().

-spec code_change(OldVsn :: term() | {down, term()}, State :: state(),
                  Extra :: term()) ->
                         {ok, NewState :: state()} | {error, Reason :: term()}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

-spec receive_results() -> [agent()].
receive_results() ->
    receive
        {result, Agents} -> Agents
    end.
