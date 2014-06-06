-module(lazy_server).

-export([
         start/0,
         stop/0,
         wait/1
        ]).

-export([loop/0, child/2]).

-spec start() -> ok. 
start() ->
    Pid = spawn(lazy_server, loop, []),
    register(lazy_server, Pid),
    ok.

-spec stop() -> ok.
stop() ->
    lazy_server ! stop,
    unregister(lazy_server),
    ok.

-spec wait(number()) -> ok.
wait(Ms) ->
    case whereis(lazy_server) of
        undefined -> throw(server_not_started);
        Pid -> Pid ! {self(), Ms}
    end,
    receive _ -> ok
    after Ms -> ok
    end.

-spec loop() -> ok.
loop() ->
    receive 
        {Pid, Ms} when is_number(Ms) -> 
            spawn(lazy_server, child, [Pid, Ms]),
            loop();
        stop -> ok
    end.   
            
-spec child(pid(), number()) -> ok.
child(Pid, Ms) ->
    receive
        _ -> ok
    after Ms ->
            Pid ! ok
                
    end.
    
    
