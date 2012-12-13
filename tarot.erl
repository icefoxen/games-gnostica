% tarot.erl
% A tarot deck module.
% ....hm.  Not quite sure how to organize the data...
% {minor|major, card|{number, suit}}

-module( tarot ).

-export( [cardToString/1, newDeck/0, deckToString/1, shuffleDeck/1,
	  addToDeck/2, drawCard/1, drawCards/2] ).

concatList( ListOfStrings ) ->
    Concat = fun( X, Y ) -> string:concat( X, Y ) end,
    lists:foldr( Concat, "", ListOfStrings ).

buildMinorArcana() ->
    List = [buildMinorArcana( discs,  1, [] ),
	    buildMinorArcana( cups,   1, [] ),
	    buildMinorArcana( rods,   1, [] ),
	    buildMinorArcana( swords, 1, [] )],
    lists:append( List ).

buildMinorArcana( Suit, 14, Accm ) ->
    Card = {minor, {14, Suit}},
    [Card | Accm];

buildMinorArcana( Suit, Number, Accm ) ->
    Card = {minor, {Number, Suit}},
    buildMinorArcana( Suit, Number + 1,
		      [Card | Accm] ).




buildMajorArcana() ->
    [{major, fool},
     {major, magician},
     {major, high_priestess},
     {major, empress},
     {major, emperor},
     {major, heirophant},
     {major, lovers},
     {major, chariot},
     {major, strength},
     {major, hermit},
     {major, wheel_of_fortune},
     {major, justice},
     {major, hanged_man},
     {major, death},
     {major, temperance},
     {major, devil},
     {major, tower},
     {major, star},
     {major, moon},
     {major, sun},
     {major, judgement},
     {major, world}].




numberToString( Number ) ->
    io:format( "~B~n", [Number] ),
    case Number of
	1 -> "Ace";
	2 -> "Two";
	3 -> "Three";
	4 -> "Four";
	5 -> "Five";
	6 -> "Six";
	7 -> "Seven";
	8 -> "Eight";
	9 -> "Nine";
	10 -> "Ten";
	11 -> "Page";
	12 -> "Knight";
	13 -> "Queen";
	14 -> "King"
    end.
	     

suitToString( Suit ) ->
    case Suit of
	cups -> "Cups";
	discs -> "Discs";
	rods -> "Rods";
	swords -> "Swords"
    end.

minorArcanaToString( {Number, Suit} ) ->
    SNumber = numberToString( Number ),
    SSuit = suitToString( Suit ),
    concatList( [SNumber, " of ", SSuit] ).

majorArcanaToString( Name ) ->
    case Name of
	fool -> "Fool";
	magician -> "Magician";
	high_priestess -> "High Priestess";
	empress -> "Empress";
	emperor -> "Emperor";
	heirophant -> "Heirophant";
	lovers -> "Lovers";
	chariot -> "Chariot";
	strength -> "Strength";
	hermit -> "Hermit";
	wheel_of_fortune -> "Wheel of Fortune";
	justice -> "Justice";
	hanged_man -> "Hanged Man";
	death -> "Death";
	temperance -> "Temperance";
	devil -> "Devil";
	tower -> "Tower";
	star -> "Star";
	moon -> "Moon";
	sun -> "Sun";
	judgement -> "Judgement";
	world -> "World"
    end.
	     

cardToString( {major, Card} ) ->
    majorArcanaToString( Card );
cardToString( {minor, Card} ) ->
    minorArcanaToString( Card ).




% This smushes everything all together, but oh well.
deckToString( Deck ) ->
    CardToString = fun( X ) -> cardToString( X ) end,
    concatList( lists:map( CardToString, Deck ) ).			     

newDeck() ->
    lists:append( buildMajorArcana(), buildMinorArcana() ).


% This builds a new list, shuffled.
% It does it by taking the nth item of a list at random.
shuffleCards( [], NewDeck ) ->
    NewDeck;

shuffleCards( OldDeck, NewDeck ) -> 
    DeckLength = length( OldDeck ),
    Card = lists:nth( random:uniform( DeckLength ),
		      OldDeck ),
    NewOldDeck = lists:delete( Card, OldDeck ),
    shuffleCards( NewOldDeck, [Card | NewDeck] ).

shuffleDeck( Deck ) ->
    shuffleCards( Deck, [] ).

addToDeck( Deck, Cards ) ->
    lists:append( Cards, Deck ).

% Hmm.  Immutable vars.
drawCard( [] ) ->
    empty_deck;
drawCard( [Card|Deck] ) ->
    {Card, Deck}.

drawCards( Deck, Count ) ->
    drawCards( Deck, Count, [] ).


drawCards( Deck, 0, Accm ) ->
    {Accm, Deck};
drawCards( Deck, Count, Accm ) ->
    [Card|NewDeck] = Deck,
    Hand = [Card|Accm],
    drawCards( NewDeck, Count - 1, Hand ).


% Returns true if card1 is larger than card2.
% Number < Royalty < Major Arcana
cardIsLarger( Card1, Card2 ) ->
    {Card1Arcana, Card1Type} = Card1,
    {Card2Arcana, Card2Type} = Card2,
    case {Card1Arcana, Card2Arcana} of
	{major, major} -> false;
	{major, minor} -> true;
	{minor, major} -> false;
	{minor, minor} ->
	    {{C1Num, _}, {C2Num, _}} = {Card1Type, Card2Type},
	     (C1Num > 10) and (C2Num <= 10)
    end.
	    
