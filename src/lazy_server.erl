-module(lazy_server).
-author('Flavio Granero <flavio@inaka.net>').

-export([start/0, wait/1, stop/0, init/0, delay/3]).

% PUBLIC METHODS
-spec start() -> ok.
start() ->
  register(?MODULE, spawn(?MODULE, init, [])),
  ok.
  
-spec stop() -> ok.
stop() ->
  case whereis(?MODULE) of
    undefined -> ok;
    Pid -> 
      exit(Pid, normal),
      unregister(?MODULE),
      ok
  end.

-spec wait(integer()) -> ok.
wait(Ms) ->
  Ref = make_ref(),
  ?MODULE ! {self(), Ref, {delay, Ms}},
  wait_loop(Ref).
  
% PRIVATE METHODS

-spec wait_loop(pid()) -> ok.
wait_loop(Ref) ->
  receive
    {Ref, done} -> ok;
    Unknown ->
      io:format("Unknown message: ~p~n",[Unknown]),
      wait_loop(Ref)
  end.

-spec init() -> pid().
init() ->
  receive 
    {Pid, MsgRef, {delay, Ms}} -> 
      spawn(?MODULE, delay, [Pid, MsgRef, Ms]),
      init();
    shutdown -> ok;
    Unknown ->
      io:format("Unknown message: ~p~n",[Unknown]),
      init()
  end.
  
- spec delay(pid(), string(), integer()) -> ok.
delay(Pid, MsgRef, Ms) ->
  receive
    _ -> delay(Pid, MsgRef, Ms)
  after Ms ->
    Pid ! {MsgRef, done},
    ok
  end.