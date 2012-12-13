% gnostica.erl
% The game itself.  Hopefully.
%
% Simon Heath
% 15/6/2006

-module( gnosticaServer ).
-export( [start/1, init/1] ).

start( ClientNode ) ->
    spawn( gnosticaServer, init, [ClientNode] ).

init( ClientNode ) ->
    receive
	_ -> io:format( "Oh my god!~n", [] )
    after 500 ->
	    {gclient, ClientNode} ! {ping, self()},
	    io:format( "Pinging...~n" )
    end,
    init( ClientNode ).
