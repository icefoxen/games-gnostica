% gnostica.erl
% The functions that model the game in general.
%
% Simon Heath
% 8/6/2006

-module( gnosticaBase ).
-export( [createGame/1, createPlayer/1, playerDrawCount/3,

	  moveLeft/1, moveRight/1, moveUp/1, moveDown/1,

	  getPlayer/2, drawCards/2, discardCards/2, removePlayer/2,
	  playerDrawToHand/3, playerDiscard/3,
	  playerAddMinion/2, playerGetStash/2,

	  gameAddMinion/6, gameRemoveMinion/6, gameGrowMinion/7,
	  gameShrinkMinion/7, gameOrientMinion/7, gameGetMinions/3,
	  gameGetTerritory/2, gameChangeTerritory/3, gameAddTerritory/3,
	  gameRemoveTerritory/2, gameGrowTerritory/3, gameShrinkTerritory/3,
	  gameMoveTerritory/3
	 ] ).


%%% COORDINATE %%%
makeCoord( X, Y ) ->
    {X, Y}.

moveLeft( {X, Y} ) ->
    {X - 1, Y}.

moveRight( {X, Y} ) ->
    {X + 1, Y}.

moveUp( {X, Y} ) ->
    {X, Y + 1}.

moveDown( {X, Y} ) ->
    {X, Y - 1}.


%%% MINIONS %%%
% Colors: red, blue, green, yellow, white, black
% Facings: n, s, e, w, up

% A minion consists of:
% Color, Size, Facing
createMinion( Color, Size, Facing ) ->
    {Color, Size, Facing}.

orientMinion( {Color, Size, _}, NewFacing ) ->
    case lists:member( NewFacing, [n,s,e,w,up] ) of
	true -> {Color, Size, NewFacing};
	false -> invalidMinionFacing
    end.

getMinionOrient( {_Color, _Size, Orient} ) ->
    Orient.

% Um.. actually, all minion growths and such are done by
% exchanging the existing one for one in your stash, so....
setMinionSize( {Color, _, Facing}, NewSize ) ->
    case (NewSize > 0) and (NewSize < 4) of
	true ->	{Color, NewSize, Facing};
	false -> invalidMinionSize
    end.

getMinionSize( {_Color, Size, _Facing} )->
    Size.


growMinion( {Color, Size, Facing} ) ->
    if
	Size == 3 ->
	    invalidMinionSize;
	true ->
	    setMinionSize( {Color, Size, Facing}, Size + 1 )
    end.

shrinkMinion( {Color, Size, Facing} ) ->
    if
	Size == 1 ->
	    invalidMinionSize;
	true ->
	    setMinionSize( {Color,Size, Facing}, Size - 1 )
    end.


    

%%% PLAYERS %%%

% A player consists of:
% Color, Minion Stash, Hand
createPlayer( Color ) ->
    MinionStash = 
	{[createMinion( Color, 1, up ),
	  createMinion( Color, 1, up ),
	  createMinion( Color, 1, up ),
	  createMinion( Color, 1, up ),
	  createMinion( Color, 1, up )],

	 [createMinion( Color, 2, up ),
	  createMinion( Color, 2, up ),
	  createMinion( Color, 2, up ),
	  createMinion( Color, 2, up ),
	  createMinion( Color, 2, up )],

	 [createMinion( Color, 3, up ),
	  createMinion( Color, 3, up ),
	  createMinion( Color, 3, up ),
	  createMinion( Color, 3, up ),
	  createMinion( Color, 3, up )]},
    
    {Color, MinionStash, []}.

playerGetColor( {Color, _Stash, _Hand} ) ->
    Color.

% Returns {NewPlayer, NewDeck}
playerDrawCount( {Color, Stash, _}, Deck, Count ) ->
    {NewHand, NewDeck} = tarot:drawCards( Deck, Count ),
    {{Color, Stash, NewHand}, NewDeck}.


playerGetStash( Player, Count ) ->
    {_, {StashOne, StashTwo, StashThree}, _} = Player,
    case Count of
	1 -> StashOne;
	2 -> StashTwo;
	3 -> StashThree;
	_ -> invalidStashSize
    end.
    
	     

% Returns number of minions in stash
playerMinionCount( {_Color, Stash, _Hand}, Size ) ->
    {One, Two, Three} = Stash,
    case Size of
	1 -> length( One );
	2 -> length( Two );
	3 -> length( Three );
	_ -> invalidMinionSize
    end.
	     
% This is a convoluted function.  Odd.  Erlang generally doesn't do those.
playerAddMinion( Player, Size ) ->
    {Color, {StashOne, StashTwo, StashThree}, Hand} = Player,
     NewMinion = createMinion( Color, Size, up ),
    case playerMinionCount( Player, Size ) of
	5 -> invalidStashSize;	    
	_ ->
	    case Size of
		1 -> {Color, {[NewMinion | StashOne], StashTwo, StashThree},
		      Hand};
		2 -> {Color, {StashOne, [NewMinion | StashTwo], StashThree},
		      Hand};
		3 -> {Color, {StashOne, StashTwo, [NewMinion | StashThree]},
		      Hand}
	    end
    end.

playerRemoveMinion( Player, Size ) ->
    {Color, {StashOne, StashTwo, StashThree}, Hand} = Player,
    case playerMinionCount( Player, Size ) of
	0 -> invalidStashSize;
	_ -> 
	    case Size of
		1 -> {Color, {tl( StashOne ), StashTwo, StashThree}, Hand};
		2 -> {Color, {StashOne, tl( StashTwo ), StashThree}, Hand};
		3 -> {Color, {StashOne, StashTwo, tl( StashThree )}, Hand}
	    end
    end.



playerGetMinion( Player, Size ) ->
    NewPlayer = playerRemoveMinion( Player, Size ),
    {NewPlayer, createMinion( playerGetColor( Player ), Size, up )}.
    


    
%%% GAME OPERATIONS %%%

% A game consists of:
% Board, Minions, Deck, Discard, Players

createGameBoard( Deck ) ->
    Coords = [{-1,-1}, {0,-1}, {1,-1},
	      {-1, 0}, {0, 0}, {1, 0},
	      {-1, 1}, {0, 1}, {1, 1}],
    {Cards, NewDeck} = tarot:drawCards( Deck, 9 ),
    {lists:zip( Coords, Cards ), NewDeck}.  % Yay lists:zip()!

    
% We do not initially have the players draw cards; it's the first
% thing they do when the game starts, along with placing a first minion.
% {board, minions-on-board, deck, discard-pile, players}
createGame( PlayerColors ) ->
    Deck = tarot:shuffleDeck( tarot:newDeck() ),
    {Board, NewDeck} = createGameBoard( Deck ),
    CreatePlayers = fun( X ) -> createPlayer( X ) end,
    Players = lists:map( CreatePlayers, PlayerColors ),
    {Board, [], NewDeck, [], Players}.


getBoard( {Board,_Minions,_Deck,_Discard,_Players} ) ->
    Board.

getMinions( {_Board,Minions,_Deck,_Discard,_Players} ) ->
    Minions.

getDeck( {_Board,_Minions,Deck,_Discard,_Players} ) ->
    Deck.

getDiscard( {_Board,_Minions,_Deck,Discard,_Players} ) ->
    Discard.


getPlayers( {_Board,_Minions,_Deck,_Discard,Players} ) ->
    Players.



% Now, I need to have functions to have players:
% draw cards, discard cards, 
% add minions, remove minions, grow and shrink minions, orient minions,
% grow territories, shrink territories, move territories, create territories,
%
% Switch hands, draw card and hold it temporarily (not in hand), choose from
% discard pile, 
%
% Limitations: One card and 3 minions per space.  Minion size 3.
% Cards and minions must exist to be used.
%
% ...I need to figure how error-checking works...
% Looks like you just return certain symbols and such.  Shibby.
%
% All functions return a new game state, and maybe some data.  Wheee, 
% functional languages.

%%% Cards %%%


drawCards( {_Board,_Minions,Deck,_Discard,_Players}, Count ) ->
    {CardsDrawn, Remaining} = lists:split( Deck, Count ),
    {{_Board, _Minions, Remaining, _Discard, _Players}, CardsDrawn}.

discardCards( {_Board,_Minions,_Deck,Discard,_Players}, Cards ) ->
    {_Board,_Minions,_Deck, [Cards | Discard],_Players}.


getPlayer( {_Board,_Minions,_Deck,_Discard,Players}, Color ) ->
    case lists:keysearch( Color, 1, Players ) of
	{value, Player} -> Player;
	false -> playerDNE
    end.


removePlayer( Game, PlayerColor ) ->
    lists:keydelete( PlayerColor, 1, getPlayers( Game ) ).

replacePlayer( Game, PlayerColor, NewPlayer ) ->
    lists:keyreplace( PlayerColor, 1, getPlayers( Game ), NewPlayer ).

playerDrawToHand( Game, PlayerColor, Count ) ->
    {Color, Stash, Hand} = getPlayer( Game, PlayerColor ),
    {NewGame, DrawnCards} = drawCards( Game, Count ),
    replacePlayer( NewGame, Color, {Color, Stash, [DrawnCards | Hand]} ).

playerDiscard( Game, PlayerColor, Cards ) -> 
    {Color, Stash, Hand} = getPlayer( Game, PlayerColor ),
    NewHand = lists:subtract( Hand, Cards ),
    NewGame = discardCards( Game, Cards ),
    {NewGame, {Color, Stash, NewHand}}.



%%% Minions %%%
% Hmmm...  I think I need a function to remove a minion from the player's
% stash, and one to add one to the board, then one to do both at once.
% I also need functions to remove a minion from the board, and add one
% to a stash.  Hm.    
%
% ...crap.  Minions don't know their own coordinates.
% I'ma gonna have to change some things...

	
gameGetMinions( Game, X, Y ) ->
    {_Board,Minions,_Deck, _Discard,_Players} = Game,
    Pred = fun( {Coord, _Minion} ) -> Coord == {X,Y} end,
    lists:filter( Pred, Minions ).

gameGetMinions( Game, X, Y, Color, Size ) ->
    {_Board, Minions, _Deck, _Discard, _Players} = Game,
    Pred = fun( {Coord, {Color2, Size2, _}} ) -> 
		   (Coord == {X,Y}) or (Color == Color2) or (Size == Size2)
	   end,
    lists:filter( Pred, Minions ).

gameAddMinion( Game, PlayerColor, Size, X, Y, Orien ) ->
    BoardMinions = gameGetMinions( Game, X, Y, PlayerColor, Size ),
    if 
	length( BoardMinions ) > 3 -> invalidMinionCount;
	true -> gameAddMinionUnsafe( Game, PlayerColor, Size, X, Y, Orien )
    end.
	     

gameAddMinionUnsafe( Game, PlayerColor, Size, X, Y, Orien ) ->
    Player = getPlayer( Game, PlayerColor ),
    {NewPlayer, Minion} = playerGetMinion( Player, Size ),
    NewMinion = orientMinion( Minion, Orien ),
    {Board,Minions,Deck, Discard,Players} = 
	replacePlayer( Game, playerGetColor( Player ), NewPlayer ),
    {Board, [{{X,Y},NewMinion}|Minions], Deck, Discard, Players}.
    
	     
						       
    
     
gameRemoveMinion( Game, PlayerColor, X, Y, Size, Orien ) ->
    Minion = {{X,Y},{PlayerColor, Size, Orien}},
    {Board, Minions, Deck, Discard, Players} = Game,
    NewMinions = list:delete( Minion, Minions ),
    {Board, NewMinions, Deck, Discard, Players}.



gameGrowMinion( Game, PlayerColor, X, Y, Size, Orien, NewOrien ) ->
    if Size == 3 -> invalidMinionSize;
       true ->
	    NewGame = gameRemoveMinion( 
			Game, PlayerColor, X, Y, Size, Orien ),
	    gameAddMinion( NewGame, PlayerColor, X, Y, (Size + 1), NewOrien )
    end.
	    
	    
	    

gameShrinkMinion( Game, PlayerColor, X, Y, Size, Orien, NewOrien ) ->
    if Size == 1 -> invalidMinionSize;
       true ->
	    NewGame = gameRemoveMinion( 
			Game, PlayerColor, X, Y, Size, Orien ),
	    gameAddMinion( NewGame, PlayerColor, X, Y, (Size - 1), NewOrien )
    end.

gameOrientMinion( Game, PlayerColor, X, Y, Size, OldOrien, NewOrien ) ->
    NewGame = gameRemoveMinion( 
		Game, PlayerColor, X, Y, Size, OldOrien ),
    gameAddMinion( NewGame, PlayerColor, X, Y, Size, NewOrien ).


gameGetTerritory( Game, Location ) ->
    Board = getBoard( Game ),
    case lists:keysearch( Location, 1, Board ) of
	{value, Terr} -> Terr;
	false -> territoryDNE
    end.
    

% XXX: What if you can't, or it's not valid, or you give a bad location?
gameChangeTerritory( Game, NewCard, Location ) ->
    {Board, Minions, Deck, Discard, Players} = Game,
    NewBoard = lists:keyreplace( Location, 1, Board, {Location, NewCard} ),
    {NewBoard, Minions, Deck, Discard, Players}.


% XXX: What if it's an invalid location, or something already exists there?
gameAddTerritory( Game, Card, Location ) ->
    {Board, Minions, Deck, Discard, Players} = Game,
    NewBoard = [{Location, Card} | Board],
    {NewBoard, Minions, Deck, Discard, Players}.

gameRemoveTerritory( Game, Location ) ->
    {Board, Minions, Deck, Discard, Players} = Game,
    NewBoard = lists:keydelete( Location, 1, Board ),
    {NewBoard, Minions, Deck, Discard, Players}.

    
    

gameGrowTerritory( Game, Location, NewCard ) ->
    OldCard = gameGetTerritory( Game, Location ),
    case tarot:cardIsLarger( NewCard, OldCard ) of
	true -> gameChangeTerritory( Game, NewCard, Location );
	false -> invalidTerritorySize
    end.

gameShrinkTerritory( Game, Location, NewCard ) ->
    OldCard = gameGetTerritory( Game, Location ),
    case tarot:cardIsLarger( NewCard, OldCard ) of
	true -> invalidTerritorySize;
	false -> gameChangeTerritory( Game, NewCard, Location )
    end.


% XXX: What if you can't?  Or there's no territory there?
gameMoveTerritory( Game, OldLocation, NewLocation ) ->
    Card = gameGetTerritory( Game, OldLocation ),
    case Card of
	false -> territoryDNE;
	_ -> Game1 = gameRemoveTerritory( Game, OldLocation ),
	     gameAddTerritory( Game1, Card, NewLocation )
    end.
    
