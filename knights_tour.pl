% Knight's tour

%
%
%
% Move with heuristic
%
%
%
knight(Tour) :-
	chessboardSize(N,M),
	Total is N*M,
	generate(Total, L),
	position(I,J),
	pos2d(I, J, P),
	changeElement(P, 1, L, L2),
	heuristicSolution(I, J, L2, 1, Solution),
	Tour = [(I, J)|Solution].

% Checks that (I,J) belongs to the chessboard.
case(I, J) :- chessboardSize(N, M), I >= 1, J >= 1, N >= I, M >= J.

% Prints the list given in a parameter
printList([]).
printList([X|L]) :- write(X), write( ,), printList(L).

% Generate a list of N elements, containing only 0.
generate(0, []) :- !.
generate(N, [0|L]) :- N1 is N-1, generate(N1, L).

% Call: changeElement(+index, +replacingValue, +inputList, -outputList).
changeElement(1, Y, [_|L], [Y|L]) :- !.
changeElement(I, Y, [X|L], [X|Z]) :-
	I1 is I-1, 
	changeElement(I1, Y, L, Z).

% Saves the I-th element of the list L in X.
% Call: element(+I, +L, -X).
element(1, [X|_], X) :- !.
element(I, [_|L], Y) :- I1 is I-1, element(I1, L, Y).

% Converts the coordinates (I,J) into a 1-dimensional index P.
% Call: pos2d( +I, +J, -P )
pos2d(I, J, P) :- chessboardSize(N,_), P is (I-1)*N+J.

% Converts the 1-dimensional index P into coordinates (I, J)
% Call: pos1d( +P, -I, -J ).
pos1d(P, I, J) :- 
	chessboardSize(N,_), 
	I is 1+ ((P-1) // N), 
	J is P - (I-1)*N.

% Returns true iff case (I,J) in chessboard L is non occupied.
emptySquare(I, J, L) :-
	case(I,J),
	pos2d(I,J,Index),
	element(Index,L,Value),
	Value =:= 0.

% Every possible knight move from (I,J) is saved into (I1, J1).
possibleKnightMove(I, J, I1, J1) :- I1 is I+1, J1 is J+2.
possibleKnightMove(I, J, I1, J1) :- I1 is I+2, J1 is J+1.
possibleKnightMove(I, J, I1, J1) :- I1 is I+2, J1 is J-1.
possibleKnightMove(I, J, I1, J1) :- I1 is I+1, J1 is J-2.
possibleKnightMove(I, J, I1, J1) :- I1 is I-1, J1 is J-2.
possibleKnightMove(I, J, I1, J1) :- I1 is I-2, J1 is J+1.
possibleKnightMove(I, J, I1, J1) :- I1 is I-2, J1 is J-1.
possibleKnightMove(I, J, I1, J1) :- I1 is I-1, J1 is J+2.

% Returns true if there is any move from (I,J) in the chessboard L.
% Used to count the number of possible moves from a square.
hasPossibleMove(I, J, L) :-
	possibleKnightMove(I, J, I1, J1), 
	emptySquare(I1,J1,L).

% Checks whether the number of possible moves from square (I,J) in chessboard L is V-1.
squareVerifiesValuation(I, J, L, V) :-
	emptySquare(I, J, L),
	setof(_, hasPossibleMove(I,J,L), L2),
	V2 is V-1, % we can't go back to the initial square
	length(L2, V2).

% From the square (I,J) in the chessboard L, returns the next square (I1, J1) which has the valuation V, if there is
% one.
hasPossibleMoveValue(I, J, L, V, I1, J1) :-
	possibleKnightMove(I, J, I1, J1),
	squareVerifiesValuation(I1,J1,L,V).

% Looks for the maximal valuation available from the current square.
seekIndexOfMaxValuation(I, J, L, ExpectedMax, RealMax) :-
	ExpectedMax =< 8, % 8 == maximal number of moves from a given square
	hasPossibleMoveValue(I, J, L, ExpectedMax, _, _), 
	RealMax is ExpectedMax.
seekIndexOfMaxValuation(I, J, L, ExpectedMax, RealMax) :-
	ExpectedMax =< 7, % as next line, we add one
	Max1 is ExpectedMax+1,
	seekIndexOfMaxValuation(I, J, L, Max1, RealMax). 

% For the last move, we don't need to seek the valuation, as we have only one empty square.
% This function returns the last possible square.
lastMove(I, J, L, I1, J1, Num) :-
	chessboardSize(N,M),
	BeforeLastMove is N*M-1,
	Num =:= BeforeLastMove,
	possibleKnightMove(I, J, I1, J1),
	emptySquare(I1,J1,L).

% Returns the optimal move from square (I,J) in chessboard L and stores the result in (I1,J1).
optimalMove(I, J, L, I1, J1) :-
	seekIndexOfMaxValuation(I, J, L, 1, RealMax),
	hasPossibleMoveValue(I, J, L, RealMax, I1, J1).

% Better move function (see the simple move above)
% Call: heuristicSolution(I, J, L, Num, Tour).
% From square (I,J) in chessboard L, applies the Num-th move and stores the move in Tour.

% For the last move, redirect to lastMove.
heuristicSolution(I, J, L, Num, [(I1, J1)]) :-
	lastMove(I, J, L, I1, J1, Num),
	!.

% For the other moves, retrieves the optimal move and go to the corresponding square, then recursively calls the
% function.
heuristicSolution(I, J, L, Num,[(I1,J1)|Tour]) :-
	optimalMove(I, J, L, I1, J1),
	pos2d(I1, J1, P), 
	changeElement(P, Num, L, L2),
	Num2 is Num+1,
	heuristicSolution(I1, J1, L2, Num2, Tour),
	!. % to stop after the first solution

%
%
%
% Simpler move;
%
%
%

naiveSolution(Tour) :-
	chessboardSize(N,M), 
	Total is N*M, 
	generate(Total, L), 
	position(I,J),
	simpleMove(I, J, L, 0, Tour).

% Returns true iff L doesn't contain any zero values.
noZeroList([]) :- !.
noZeroList([X|L]) :- X \== 0, noZeroList(L).

% Tries to move the knight from (I,J) in the chessboard L and to store the resulting chessboard in L2.
% The number of moves is incremented from Num to Num2.
tryMove(I, J, L, Num, L2, Num2) :- 
	case(I,J), 
	pos2d(I, J, Index),
	element(Index, L, Value),
	Value =:= 0, % Square is empty
	Num2 is Num+1,
	changeElement(Index, Num2, L, L2).

% Moves from the square (I,J) in chessboard L (Num-th move), stores the position and tries a next move.
simpleMove(I, J, L, Num, [(I,J)]) :- 
	tryMove(I,J,L,Num,L2,_), 
	noZeroList(L2), 
	!.
simpleMove(I, J, L, Num, [(I,J)|Tour]) :- 
	tryMove(I, J, L, Num, L2, Num2),
	nextSimpleMove(I, J, L2, Num2, Tour),
	!. % only one solution

% Tries to move the knight in every possible direction.
nextSimpleMove(I, J, L, Num, Tour) :- 
	possibleKnightMove(I, J, I1, J1),
	simpleMove(I1,J1,L, Num,Tour).

%
%
%
% IHM
%
%
%	
	
showSolution(Tour) :-
	chessboardSize(N, _),
	chessboard('knight', N),
	simul(Tour).
	
save([], _).
save([(X,Y)|L], I) :- Term =.. [tour, I, X, Y], assert(Term), J is I+1 ,save(L, J).
	
simul(L) :-
	save(L, 1),
	tour(1, X, Y),
	drawknight(X,Y),
	new(@go, dialog('Next step')),
	new(@previousStep, button(previous_step)),
	send(@previousStep, message, message(@prolog, previousStep, 1)),
	new(@nextStep, button(next_step)),
	send(@nextStep, message, message(@prolog, nextStep, 1)),
	send(@go, append, @previousStep),
	send(@go, append, @nextStep),
	send(@go, open).
	
column(_, 0).
column(I, J) :- J >= 1, Term =.. [case1, I, J], assert(Term), J1 is J-1, column(I, J1).
grid(0, _).
grid(I, J) :- I >= 1, column(I, J), I1 is I-1, grid(I1, J).
generateGrid(N) :- grid(N,N).

colorCase1(X, Y) :- 
	Y mod 2 =:= 1, X mod 2 =:= 1, 
	new(B, box(20, 20)),
	send(@grid, display, B, point((X-1)*20, (Y-1)*20)),
	send(B, fill_pattern, colour(black)).
colorCase1(X, Y) :- 
	Y mod 2 =:= 0, X mod 2 =:= 0, 
	new(B, box(20, 20)),
	send(@grid, display, B, point((X-1)*20, (Y-1)*20)),
	send(B, fill_pattern, colour(black)).
colorCase1(X, Y) :- 
	new(B, box(20, 20)),
	send(@grid, display, B, point((X-1)*20, (Y-1)*20)),
	send(B, fill_pattern, colour(white)).

takecase1(X, Y) :- 
	Y mod 2 =:= 1, X mod 2 =:= 1, 
	new(B, box(20, 20)),
	send(@grid, display, B, point((X-1)*20, (Y-1)*20)),
	send(B, fill_pattern, colour(red)).
takecase1(X, Y) :- 
	Y mod 2 =:= 0, X mod 2 =:= 0,
	new(B, box(20, 20)),
	send(@grid, display, B, point((X-1)*20, (Y-1)*20)),
	send(B, fill_pattern, colour(red)).
takecase1(X, Y) :- 
	new(B, box(20, 20)),
	send(@grid, display, B, point((X-1)*20, (Y-1)*20)),
	send(B, fill_pattern, colour(blue)).

drawknight(X, Y) :- 
	new(B, box(20, 20)),
	send(@grid, display, B, point((X-1)*20, (Y-1)*20)),
	send(B, fill_pattern, colour(yellow)).
	
chessboard(T, N) :-
	generateGrid(N),
	Term =.. [size, N],
	assert(Term),
	new(@grid, window(T)),
	send(@grid, open),
	send(@grid, width(N*20)),
	send(@grid, height(N*20)),
	forall(case1(X, Y),colorCase1(X, Y)).
	
setSize :-
	new(Dialog, dialog('Parametres')),
	send_list(Dialog, append,
		[ 	new(T, text_item(titre)),
			new(S1, int_item(taille_chessboard, low := 1, high := 40)),
			button(creer, and(message(@prolog,chessboard,T?selection,S1?selection), 
			message(Dialog, destroy)))
		]),
		send(Dialog, default_button, creer),
		send(Dialog, open).
	
nextStep(I) :-
	tour(I, X1, Y1),
	J is I+1,
	tour(J, X2, Y2),
	takecase1(X1, Y1),
	drawknight(X2, Y2),
	send(@nextStep, message, message(@prolog, nextStep, J)),
	send(@previousStep, message, message(@prolog, previousStep, J)).

previousStep(I) :-
	I > 1,
	J is I-1,
	tour(I, X1, Y1),
	tour(J, X2, Y2),
	colorCase1(X1,Y1),
	drawknight(X2,Y2),
	send(@nextStep, message, message(@prolog, nextStep, J)),
	send(@previousStep, message, message(@prolog, previousStep, J)).
	
%
%
%
% - Part launched at the beginning
%
%
%
:-  write('Enter the number of lines of the chessboard: '),
	read(N),
	write('Enter the number of columns of the chessboard: '),
	read(M),
	write('Enter the initial position (I,J) of the knight: I='),
	read(I),
	write('J='),
	read(J),
	TermChessboardSize =.. [config, N, M],
	TermPosition =.. [position, I, J],
	assert(TermChessboardSize),
	assert(TermPosition).
	
go:-
	knight(P),
	showSolution(P).
