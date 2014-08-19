-module(lazy_server).

-export([
         start/0,
         stop/0,
         wait/1
        ]).

-export([loop/0, sleep/2]).

-spec start() -> ok.
start() ->
    Pid = spawn(lazy_server, loop, []),
    register(loop, Pid),
    ok.

-spec stop() -> ok.
stop() ->
    loop ! stop,
    unregister(loop),
    ok.

-spec wait(number()) -> ok.
wait(Ms) ->
    case whereis(loop) of
        undefined -> throw(server_not_started);
        _ -> loop ! {self(), Ms}
    end,
    receive
        finished -> ok
    end.

-spec loop() -> ok.
loop() ->
    receive
        {PidW, Ms} -> 
            spawn(lazy_server, sleep, [PidW, Ms]),
            loop();
        stop -> ok
    end.

-spec sleep(pid(), number()) -> ok.
sleep(PidW, Ms) ->
    receive
    after Ms ->
        PidW ! finished
    end.