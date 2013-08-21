%% @author jstypka <jasieek@student.agh.edu.pl>
%% @version 1.1
%% @doc Glowny modul aplikacji implementujacy logike procesu zarzadzajacego algorytmem.

-module(emas).
-export([run/0]).

%% ====================================================================
%% API functions
%% ====================================================================

%% @spec run(int()) -> ok
%% @doc Funkcja uruchamiajaca algorytm dla wpisanych parametrow (config.erl)
run() ->
  Instancja = "instancja",
  init(Instancja),
  {Time,{Result,Pids}} = timer:tc(fun spawner/1, [Instancja]),
  cleanup(Pids),
  io:format("Total time:   ~p s~nFitness:     ~p~n",[Time/1000000,Result]).

%% ====================================================================
%% Internal functions
%% ====================================================================

%% @spec init() -> ok
%% @doc Funkcja wykonujaca wszelkie operacje potrzebne przed uruchomieniem
%% algorytmu.
init(Instancja) ->
  register(supervisor,self()),
  file:make_dir(Instancja).

%% @spec cleanup(List) -> ok
%% @doc Funkcja sprzatajaca po zakonczonym algorytmie, dostajaca jako
%% argument liste uruchomionych ciagle procesow.
cleanup(Pids) ->
  ok = emas_util:rambo(Pids),
  emas_util:checkIfDead(Pids),
  emas_util:clearInbox(),
  unregister(supervisor).

%% @spec spawner(int()) -> {float(),List}
%% @doc Funkcja spawnujaca wyspy, ktorych ilosc jest okreslona w argumencie.
%% Zwracany jest wynik obliczen i lista Pid.
spawner(Instancja) ->
  PidsRefs = [spawn_monitor(island,proces,[Instancja,X]) || X <- lists:seq(1,config:islandsNr())],
  {Pids,_} = lists:unzip(PidsRefs),
  receiver(Pids).

%% @spec receiver(List1) -> {float() | timeout,List2}
%% @doc Funkcja odbierajaca wiadomosci od wysp (z wynikami) i obslugujaca je.
%% Zwracany jest koncowy wynik wraz z lista uruchomionych procesow.
receiver(Pids) ->
  receive
    {agent,_From,Agent} ->
      Index = random:uniform(length(Pids)),
      lists:nth(Index, Pids) ! {agent,self(),Agent},
      receiver(Pids);
    {result,Result} ->
      Precision = config:stopPrec(),
      if Result < -Precision ->
        receiver(Pids);
        Result >= -Precision ->
          {Result,Pids}
      end;
    {'DOWN',_Ref,process,Pid,Reason} ->
      io:format("Proces ~p zakonczyl sie z powodu ~p~n",[Pid,Reason]),
      {NewPid,_Ref} = spawn_monitor(emas,proces,[]),
      io:format("Stawiam kolejna wyspe o Pid ~p~n",[NewPid]),
      receiver([NewPid|lists:delete(Pid,Pids)])
  after config:timeout() ->
    {timeout,Pids}
  end.

