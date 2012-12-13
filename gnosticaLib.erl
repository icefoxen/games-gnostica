% gnosticaLib.erl
% General library functions.

concatList( ListOfStrings ) ->
    Concat = fun( X, Y ) -> string:concat( X, Y ) end,
    lists:foldr( Concat, "", ListOfStrings ).


listFind( [], Pred ) ->
    false;
listFind( [Hd|Tl], Pred ) ->
    if
	Pred( Hd ) ->
	    Hd;
	Else ->
	    listFind( Tl, Pred )
    end.
