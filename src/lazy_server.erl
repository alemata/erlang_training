-module(lazy_server).
-author('federico.carrone@gmail.com').
-export([start/0, stop/0, wait/1, loop/0, sleep/2]).

-spec start() -> ok.
start() ->
  register(?MODULE, spawn(?MODULE, loop, [])),
  ok.

-spec stop() -> ok.
stop() ->
  unregister(?MODULE),
  ok.

-spec wait(integer()) -> ok.
wait(Ms) ->
  ?MODULE ! {self(), wait, Ms},
  receive
    finished ->
    ok
  end.

-spec loop() -> none().
loop() ->
  receive
  {From, wait, Ms} ->
    spawn(?MODULE, sleep, [From, Ms]),
    loop()
  end.

-spec sleep(pid(), integer()) -> none().
sleep(From, Ms) ->
  receive
  after Ms ->
    From ! finished
end.