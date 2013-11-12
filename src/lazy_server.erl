-module(lazy_server).
-author('federico.carrone@gmail.com').
-export([start/0, stop/0, wait/1]).

-spec start() -> ok.
start() ->
  register(?MODULE, self()),
  ok.

-spec stop() -> ok.
stop() ->
  unregister(?MODULE),
  ok.

-spec wait(integer()) -> ok.
wait(Ms) ->
  sleep(?MODULE, Ms),
  receive
    {finished, Ms} ->
    ok
  end.

sleep(From, Ms) ->
  receive
  after Ms ->
    From ! {finished, Ms}
end.