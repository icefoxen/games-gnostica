% gnosticaClient.erl
% Yay, now I have a GUI mock-up!
% Come to think of it, a real talking server and client would be more useful.
%
% Simon Heath
% 9/6/2006

-module( gnosticaClient ).
-export( [start/0, init/0] ).

start() ->
    register( gclient, spawn( gnosticaClient, init, [] ) ).

init() ->
    GC = gs:start(),
    gs:window( mainwin, GC, [{title, "Gnostica Client"}, {map, true},
			     {configure,true}, {width, 640}, {height, 480}] ),
    
    % main frame!
    gs:frame( mainframe, mainwin,[{packer_x,
				    [{stretch,6}, {stretch,1}]},
				   {packer_y,
				    [{stretch,1},{stretch,1},{stretch,1},
				     {stretch,1},{stretch,1},{stretch,1},
				     {stretch,1}, {stretch,1}]},
				   {width, 400}, {bg,red},
				 {buttonpress,true}]
				  ),
    % Buttons!
    gs:button( b1, mainframe, [{label,{text,"card1"}},{pack_xy,{2,1}}] ),
    gs:button( b2, mainframe, [{label,{text,"card2"}},{pack_xy,{2,2}}] ),
    gs:button( b3, mainframe, [{label,{text,"card3"}},{pack_xy,{2,3}}] ),
    gs:button( b4, mainframe, [{label,{text,"card4"}},{pack_xy,{2,4}}] ),
    gs:button( b5, mainframe, [{label,{text,"card5"}},{pack_xy,{2,5}}] ),
    gs:button( b6, mainframe, [{label,{text,"card6"}},{pack_xy,{2,6}}] ),

    % Main canvas
    % Hmmm... events and objects and such.  {buttonpress,true} is my friend.
    % 'Twill be interesting.
    gs:canvas( maincanvas, mainframe, [{pack_xy,{1,{1,6}}}, {bg,blue}, {buttonpress,true}] ),

    % Minion stash (?)
    gs:canvas( minioncanvas, mainframe, [{pack_xy,{1,7}}, {bg,green}] ),

    % Bottom textbox
    gs:editor( messagebox, mainframe, [{pack_xy,{1,8}}, {vscroll,right}] ),
    
    % Bottom-right frame
    gs:frame( brframe, mainframe, [{pack_xy,{2,{7,8}}}] ),
    gs:config( mainframe, [{width,640},{height,480}] ),
    mainloop().


mainloop() ->
    mainloop( 0 ).

mainloop( PingCount ) ->
    receive
        {gs,_Id,destroy,_Data,_Arg} -> bye;
        %{gs,_Id,configure,_Data,[W,H|_]} ->
	%    io:format( "Wheeee!  ~p~n", [] ),
        %    gs:config( mainframe, [{width,W},{height,H}]); % repack
	{ping, Pid} ->
	    io:format( "Got ping number ~p from ~p~n", [PingCount, Pid] ),
	    mainloop( PingCount + 1 );
        Other ->
            io:format( "loop got: ~p~n",[Other] )
    end,
    mainloop( PingCount ).
