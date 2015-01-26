-module(mas_conc_arena).
-behaviour(gen_server).

%% API
-export([start_link/4, give_arenas/2, call/2, close/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2,
         handle_info/2, terminate/2, code_change/3]).

-include ("mas.hrl").

-record(state, {supervisor :: pid(),
                waitlist = [] :: list(),
                agent_pids = [] ::[pid()],
                arenas :: dict:dict(),
                interaction :: atom(),
                sim_params :: sim_params(),
                config :: config()}).

-define(CLOSING_TIMEOUT, 2000).
-define(AGENT_THRESHOLD, 2). %% TODO this should be user-configurable and use case dependent

-type agent() :: mas:agent().
-type sim_params() :: mas:sim_params().
-type arenas() :: mas_conc_supervisor:arenas().

%%%===================================================================
%%% API
%%%===================================================================

-spec start_link(pid(), atom(), sim_params(), config()) -> pid().
start_link(Supervisor, migration, _SP, Cf) ->
    {ok, Pid} = mas_conc_port:start_link(Supervisor, Cf),
    Pid;

start_link(Supervisor, Interaction, SP, Cf) ->
    {ok, Pid} =
        gen_server:start_link(?MODULE, [Supervisor, Interaction, SP, Cf], []),
    Pid.

%% @doc Sends a request with given agent to this arena
-spec call(pid(), agent()) -> agent() | close.
call(Pid, Agent) ->
    gen_server:cast(Pid, {interact, self(), Agent}),
    receive
        {response, Reply} ->
            Reply
    end.

-spec give_arenas(pid(), arenas()) -> ok.
give_arenas(Pid, Arenas) ->
    gen_server:call(Pid, {arenas, Arenas}, infinity).

-spec close(pid()) -> ok.
close(Pid) ->
    gen_server:cast(Pid, close).

%%%===================================================================
%%% Callbacks
%%%===================================================================

-spec init(Args :: term()) -> {ok, State :: #state{}} |
                              {ok, State :: #state{}, timeout() | hibernate} |
                              {stop, Reason :: term()} | ignore.
init([Supervisor, Interaction, SP, Cf]) ->
    mas_misc_util:seed_random(),
    {ok, #state{supervisor = Supervisor,
                interaction = Interaction,
                sim_params = SP,
                config = Cf}}.


-spec handle_call(Request :: term(),
                  From :: {pid(), Tag :: term()},
                  State :: #state{}) ->
                         {reply, Reply :: term(), NewState :: #state{}} |
                         {reply, Reply :: term(), NewState :: #state{},
                          timeout() | hibernate} |
                         {noreply, NewState :: #state{}} |
                         {noreply, NewState :: #state{},
                          timeout() | hibernate} |
                         {stop, Reason :: term(), Reply :: term(),
                          NewState :: #state{}} |
                         {stop, Reason :: term(), NewState :: #state{}}.

handle_call({arenas, Arenas}, _From, St = #state{config = Cf}) ->
    {reply, ok, St#state{arenas = Arenas}, Cf#config.arena_timeout}.

-spec handle_cast(Request :: term(), State :: #state{}) ->
                         {noreply, NewState :: #state{}} |
                         {noreply, cleaning, timeout() | hibernate} |
                         {noreply, NewState :: #state{}, timeout() | hibernate}|
                         {stop, Reason :: term(), NewState :: #state{}}.
handle_cast({interact, Pid, _Agent}, cleaning) ->
    Pid ! {response, the_end},
    {noreply, cleaning, ?CLOSING_TIMEOUT};

handle_cast({interact, Pid, Agent}, St = #state{config = Cf}) ->
    Waitlist = [Agent | St#state.waitlist],
    Pids = [Pid | St#state.agent_pids],
    case length(Waitlist) of
        ?AGENT_THRESHOLD ->
            NewState = perform_interaction(Waitlist, Pids, St),
            {noreply, NewState};
        _ ->
            {noreply,
             St#state{agent_pids = Pids, waitlist = Waitlist},
             Cf#config.arena_timeout}
    end;

handle_cast(close, _State) ->
    {noreply, cleaning, ?CLOSING_TIMEOUT}.


-spec handle_info(Info :: timeout() | term(), State :: #state{})
                 -> {noreply, NewState :: #state{}} |
                    {noreply, NewState :: #state{}, timeout() | hibernate} |
                    {stop, Reason :: term(), NewState :: #state{}}.
handle_info(timeout, cleaning) ->
    {stop, normal, cleaning};

handle_info(timeout, St = #state{waitlist = Waitlist, agent_pids = Pids}) ->
    case length(Waitlist) of
        0 ->
            {noreply, St};
        _ ->
            io:format("Performing a defective interaction~n"),
            NewState = perform_interaction(Waitlist, Pids, St),
            {noreply, NewState}
    end.

-spec terminate(Reason :: (normal | shutdown | {shutdown, term()} | term()),
                State :: #state{}) -> term().
terminate(_Reason, _State) ->
    ok.

-spec code_change(OldVsn :: term() | {down, term()}, State :: #state{},
                  Extra :: term()) ->
                         {ok, NewState :: #state{}} | {error, Reason :: term()}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

-spec perform_interaction([agent()], [pid()], #state{}) -> #state{}.
perform_interaction(Waitlist, Pids, St) ->
    {Cf, SP} = {St#state.config, St#state.sim_params},
    NrOfAgents = length(Waitlist),

    NewAgents = mas_misc_util:meeting_proxy({St#state.interaction, Waitlist},
                                            mas_concurrent,
                                            SP,
                                            Cf),

    respond(NewAgents, Pids, St#state.arenas, SP, Cf),

    exometer:update([St#state.supervisor, St#state.interaction], NrOfAgents),

    St#state{waitlist = [], agent_pids = []}.


-spec respond([agent()], [pid()], arenas(), sim_params(), config()) -> list().
respond(Agents, Pids, Arenas, SP, Cf) when length(Agents) >= length(Pids) ->
    [Pid ! {response, Agent}
        || {Pid, Agent} <- mas_misc_util:shortest_zip(Pids, Agents)],

    [spawn(mas_conc_agent, start, [Agent, Arenas, SP, Cf])
        || Agent <- lists:nthtail(length(Pids), Agents)];

respond(Agents, Pids, _Arenas, _SP, _Cf) when length(Agents) =< length(Pids) ->
    [Pid ! {response, Agent}
        || {Pid, Agent} <- mas_misc_util:shortest_zip(Pids, Agents)],

    [Pid ! {response, close}
        || Pid <- lists:nthtail(length(Agents), Pids)].