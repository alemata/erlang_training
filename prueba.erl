-module(prueba).
-export([fac/1]).

fac(0) -> 1;
%% Porque guarda n > 0 en ejemplo
fac(N) -> N*fac(N-1).
