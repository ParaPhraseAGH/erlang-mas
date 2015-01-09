%% @author jstypka <jasieek@student.agh.edu.pl>
%% @version 1.0

-module(mas_conc_supervisor).
-behaviour(gen_server).

-include("mas.hrl").

%% API
-export([start/2, go/1, close/1]).
%% gen_server
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
         code_change/3]).
-export_type([arenas/0]).

-type sim_params() :: mas:sim_params().
-type arenas() :: dict:dict(mas:agent_behaviour(), pid()).

%% ====================================================================
%% API functions
%% ====================================================================
-spec start(sim_params(), config()) -> pid().
start(SP, Cf) ->
    {ok,Pid} = gen_server:start(?MODULE, [SP, Cf], []),
    Pid.

-spec go(pid()) -> ok.
go(Pid) ->
    gen_server:cast(Pid, go).

-spec close(pid()) -> ok.
close(Pid) ->
    gen_server:call(Pid, close, infinity).

%% ====================================================================
%% Callbacks
%% ====================================================================
-record(state, {arenas  = dict:new() :: arenas(),
                sim_params :: sim_params(),
                config :: config()}).
-type state() :: #state{}.


-spec init(term()) -> {ok,state()} |
                      {ok,state(),non_neg_integer()}.
init([SP, Cf = #config{write_interval = Int}]) ->
    mas_misc_util:seed_random(),
    Interactions = mas_misc_util:determine_behaviours(Cf),
    ArenaList = [{Interaction, mas_conc_arena:start_link(self(),
                                                         Interaction,
                                                         SP,
                                                         Cf)}
                 || Interaction <- Interactions],

    exometer:new([self(), msg_queue_length],
                 mas_msg_queue_len,
                 [{sample_interval, Int}, {arena_pids, ArenaList}]),
    exometer_report:subscribe(mas_reporter,
                              [self(), msg_queue_length],
                              msg_lengths,
                              Int),
    Arenas = dict:from_list(ArenaList),
    [ok = mas_conc_arena:giveArenas(Pid, Arenas)
     || {_Interaction, Pid} <- ArenaList],
    mas_io_util:printArenas(ArenaList),
    {ok, #state{arenas = Arenas, config = Cf, sim_params = SP}}.


-spec handle_call(term(),{pid(),term()},state()) ->
                         {reply,term(),state()} |
                         {reply,term(),state(),hibernate | infinity | non_neg_integer()} |
                         {noreply,state()} |
                         {noreply,state(),hibernate | infinity | non_neg_integer()} |
                         {stop,term(),term(),state()} |
                         {stop,term(),state()}.
handle_call(close, _From, St) ->
    [mas_conc_arena:close(Pid) || {_Name,Pid} <- dict:to_list(St#state.arenas)],
    exometer_report:unsubscribe_all(mas_reporter, [self(), msg_queue_length]),
    exometer:delete([self(), msg_queue_length]),
    {stop, normal, ok, St}.

-spec handle_cast(term(),state()) -> {noreply,state()} |
                                     {noreply,state(),hibernate | infinity | non_neg_integer()} |
                                     {stop,term(),state()}.

handle_cast(go, St = #state{config = Cf, sim_params = SP, arenas = Arenas}) ->
    Agents = mas_misc_util:generate_population(SP, Cf),
    _InitPopulation = [spawn(mas_conc_agent, start, [A, Arenas, SP, Cf])
                       || A <- Agents],
    {noreply, St}.


-spec handle_info(term(),state()) -> {noreply,state()} |
                                     {noreply,state(),hibernate | infinity | non_neg_integer()} |
                                     {stop,term(),state()}.
handle_info(timeout, State) ->
    {stop, timeout, State}.


-spec terminate(term(),state()) -> no_return().
terminate(_Reason, _State) ->
    ok.


-spec code_change(term(),state(),term()) -> {ok, state()}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
