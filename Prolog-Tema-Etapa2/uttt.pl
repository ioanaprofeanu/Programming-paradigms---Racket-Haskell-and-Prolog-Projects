:- dynamic detailed_mode_disabled/0.
:- ensure_loaded('files.pl').
% student file for Ultimate Tic Tac Toe implementation

% initialState/1
% initialState(-State)
% Este adevărat pentru starea inițială a jocului.

initialState([[''],['', '', '', '', '', '', '', '', ''], ['', '', '', '', '', '', '', '', ''], ['', '', '', '', '', '', '', '', ''], ['', '', '', '', '', '', '', '', ''], ['', '', '', '', '', '', '', '', ''], ['', '', '', '', '', '', '', '', ''], ['', '', '', '', '', '', '', '', ''], ['', '', '', '', '', '', '', '', ''], ['', '', '', '', '', '', '', '', '']]).

% getBoards/2
% getBoards(+State, -Boards)
% Este adevărat dacă în starea State, informațiile din tablele individuale sunt
% cele din variabila Boards.
% Boards este legată la o listă de 9 elemente, fiecare element reprezentând o tablă.
% Ordinea tablelor este cea din lista positions (din utils.pl).
% Fiecare element din listă este o listă de 9 elemente, reprezentând
% pozițiile de pe tablă, ca x, 0, sau ''.
% Pozițiile sunt în ordinea din lista positions (din utils.pl).

getBoards([H|State], Boards) :- State = Boards.

% getBoard/3
% getBoard(+State, +UPos, -Board)
% Este adebărat dacă în starea State, la poziția UPos din tabla de UTTT,
% se află tabla individuală cu reprezentarea din Board.
% Reprezentarea tablei este descrisă în predicatul getBoards/2.

% pt gasire index element
indexOf([Element|_], Element, 0).
indexOf([_|Tail], Element, Index):-
  indexOf(Tail, Element, Index1),
  Index is Index1+1.

getBoard([H|State], UPos, Board) :- 
    %% verific daca la pozitia Position din lista de liste State se afla lista Board
    nth0(Position, State, Board), 
    %% iau pozitia la care se afla Upos in lista de pozitii
    indexOf([nw, n, ne, w, c, e, sw, s, se], UPos, Position).

% getUBoard/2
% getUBoard(stare(+Board, +UboardState, +Player, +NextMoves),
% -UboardState)
% Întoarce reprezentarea UBoard-ului, indicând tablele individuale câștigate,
% remizate, sau încă în desfășurare. Reprezentarea este aceeași ca a tablelor
% individuale (vezi getBoards/2).
% remise(H1, List) :- 

getUBoard([Mov], []).
getUBoard([Mov, H1|State], [H2|Uboard]) :- (player_wins(H2, H1); (member('', H1), H2 = ''); H2 = r), getUBoard([Mov|State], Uboard), !.

% getPos/4
% getPos(+State, +UPos, +Pos, -Cell).
% Este adevărat dacă în starea State, în tabla individuală de la poziția UPos în UBoard,
% la poziția Pos pe tablă, se află simbolul Cell (x, 0, sau '').
getPos([Mov|State], UPos, Pos, Cell) :- getPos(IndivBoard, Pos, Cell),
                                        indexOf(State, IndivBoard, Position),
                                        indexOf([nw, n, ne, w, c, e, sw, s, se], UPos, Position).

% getPos/3
% getPos(+Board, +Pos, -Cell).
% Este adevărat dacă în tabla individuală reprezentată în Board, la poziția Pos, 
% se află simbolul Cell (x, 0, sau ''). Predicatul poate fi folosit și pentru UBoard, caz 
% în care Cell poate fi și r.

getPos(Board, Pos, Cell) :-
    %% verific daca la pozitia Position din lista Board se afla elementul Cell
    nth0(Position, Board, Cell), 
    %% iau pozitia la care se afla Pos in lista de pozitii
    indexOf([nw, n, ne, w, c, e, sw, s, se], Pos, Position).

% getNextPlayer/2
% getNextPlayer(+State), -NextPlayer)
% Este adevărat dacă în starea State, jucătorul care urmează este NextPlayer
% (poate fi x sau 0)..
count([],X,0).
count([X|T],X,Y):- count(T,X,Z), Y is 1+Z.
count([X1|T],X,Z):- X1\=X,count(T,X,Z).

countX0([], Cell, 0).
countX0([X|State], Cell, Total) :- countX0(State, Cell, Total2), count(X, Cell, Total3), Total is Total2 + Total3.

getNextPlayer([Mov|State], NextPlayer) :- countX0(State, x, TotalX), countX0(State, 0, Total0), NextPlayer = 0, TotalX \= Total0, !; NextPlayer = x.

% getNextAvailableBoards/2
% getNextAvailableBoards(+State, -NextBoardsPoss)
% Este adevărat dacă în starea State, pozițiile din NextBoardsPoss sunt pozițiile 
% din UBoard ale tablelor disponibile pentru următoarea mutare.

getNextAvailableBoards([Mov|State], NextBoardsPoss) :- Mov = [''], NextBoardsPoss = Positions, positions(Positions), !.
getNextAvailableBoards([Mov|State], NextBoardsPoss) :- head(Mov, MovHead), getUBoard([Mov|State], UBoard), getPos(UBoard, MovHead, ''), NextBoardsPoss = Mov, !.
getNextAvailableBoards([Mov|State], NextBoardsPoss) :- positions(Positions), getUBoard([Mov|State], UBoard), availableBoards(UBoard, Positions, Pos), NextBoardsPoss = Pos, !.

availableBoards([], [], []).
availableBoards([H1|UBoard], [H3|Positions], [H2|Res]) :- H1 = '', H2 = H3, availableBoards(UBoard, Positions, Res), !;
H1 \= '', availableBoards(UBoard, Positions, [H2|Res]), !.

% getBoardResult/2
% getBoardResult(+Board, -Result)
% Este adevărat dacă pentru o tablă individuală (sau UBoard) cu reprezentarea
% din Board, rezultatul este Result. Result poate fi:
% x sau 0, dacă jucătorul respectiv a câștigat jocul pe tabla dată;
% r, dacă s-a ajuns la remiză (toate pozițiile au fost completate dar
% tabla nu a fost câștigată);
% '', dacă tabla nu a fost câștigată și nu s-au completat toate pozițiile.
% NOTĂ: este deja definit predicatul player_wins/2 în utils.pl.
getBoardResult(Board, Result) :- player_wins(Result, Board), !; (member('', Board), Result = '', !); Result = r, !.

% buildState/3
% buildState(+Boards, +PreviousPos, -State)
% Este adevărat dacă starea State corespunde stării jocului în care tablele
% individuale sunt cele din lista Boards, iar ultima mutare a fost în 
% poziția PreviousPos într-o tablă individuală.
% NOTĂ: nu contează în care tablă individuală s-a realizat ultima mutare.

%% myConcat/3
%% myConcat(?List1, ?List2, ?List)
myConcat([],L,L).
myConcat([H1|T1],L2,[H1|TSol]) :- myConcat(T1, L2, TSol).

buildState(Boards, PreviousPos, State) :- myConcat([[PreviousPos]], Boards, State).

% validMove/2
% validMove(+State, +Move)
% Este adevărat dacă mutarea Move este legală în starea State.
% Move este fie o poziție, în cazul în care este o singură tablă disponibilă
% pentru a următoarea mutare din starea State, fie o pereche de poziții, altfel.

%(Player = ''),  getBoardResult(UBoard, Player), getUBoard(State, UBoard), !

validMove(State, Move) :- getUBoard(State, UBoard), getBoardResult(UBoard, Player), Player = '', getNextAvailableBoards(State, AvailBoards), lengthIsOne(AvailBoards),
head(AvailBoards, BoardPos), getBoard(State, BoardPos, Board), getPos(Board, Move, '').

validMove(State, (UBoardPos, BoardPos)) :- getUBoard(State, UBoard), getBoardResult(UBoard, Player), Player = '', getNextAvailableBoards(State, AvailBoards),
member(UBoardPos, AvailBoards), getBoard(State, UBoardPos, Board), getPos(Board, BoardPos, '').

lengthIsOne(List) :- lungime(List, Len), Len = 1.

lungime([],0).
lungime([_|R], N):- lungime(R,N1), N is N1 + 1.

% makeMove/3
% makeMove(+State, +Move, -NewState)
% Este adevărat dacă în urma aplicării mutării Move în starea State
% rezulta starea NewState.
% Move este fie o poziție (din lista positions), în cazul în care nu sunt mai 
% multe table disponibile pentru a următoarea mutare din starea State,
% fie o pereche de poziții, altfel.
%
% Hint: folosiți validMove pentru a verifica mutarea și buildState pentru a construi o stare.
makeMove([Movs|State], Move, NewState) :- validMove([Movs|State], Move), getNextAvailableBoards([Movs|State], AvailBoards),
lengthIsOne(AvailBoards), head(AvailBoards, BoardPos), indexOf([nw, n, ne, w, c, e, sw, s, se], BoardPos, UPos),
indexOf([nw, n, ne, w, c, e, sw, s, se], Move, Pos), getNextPlayer([Movs|State], NextPlayer), getSimilarity(State, State2, UPos, Pos, 0, NextPlayer), buildState(State2, Move, NewState).

makeMove([Movs|State], (UBoardPos, BoardPos), NewState) :- validMove([Movs|State], (UBoardPos, BoardPos)), indexOf([nw, n, ne, w, c, e, sw, s, se], UBoardPos, UPos),
indexOf([nw, n, ne, w, c, e, sw, s, se], BoardPos, Pos), getNextPlayer([Movs|State], NextPlayer), getSimilarity(State, State2, UPos, Pos, 0, NextPlayer), buildState(State2, BoardPos, NewState). 

getSimilarity([], [], Except, Except2, Index, Replace).
getSimilarity([H1|L1], [H1|L2], Except, Except2, Index, Replace) :- Index \= Except, Index2 is Index + 1, getSimilarity(L1, L2, Except, Except2, Index2, Replace).
getSimilarity([H1|L1], [H2|L2], Except, Except2, Index, Replace) :- Index = Except, getSimilarity2(H1, Res, Except2, 0, Replace), H2 = Res, Index2 is Index + 1, getSimilarity(L1, L2, Except, Except2, Index2, Replace).

getSimilarity2([], [], Except, Index, Replace).
getSimilarity2([H1|L1], [H1|L2], Except, Index, Replace) :- Index \= Except, Index2 is Index + 1, getSimilarity2(L1, L2, Except, Index2, Replace).
getSimilarity2([H1|L1], [H2|L2], Except, Index, Replace) :- Index = Except, H2 = Replace, Index2 is Index + 1, getSimilarity2(L1, L2, Except, Index2, Replace).

% dummy_first/2
% dummy_first(+State, -NextMove)
% Predicatul leagă NextMove la următoarea mutare pentru starea State.
% Strategia este foarte simplă: va fi aleasă cea mai din stânga-sus mutare posibilă
% (prima din lista de poziții disponibile).

head([H|_], H).

tail([_|T], T).

last(X,[X]).
last(X,[_|T]) :- last(X,T).

dummy_first(State, NextMove) :- getNextAvailableBoards(State, AvailBoards), lengthIsOne(AvailBoards), head(AvailBoards, UPos), getBoard(State, UPos, Board), nth0(Index, Board, ''), nth0(Index, [nw, n, ne, w, c, e, sw, s, se], Pos),NextMove = Pos, !.
dummy_first(State, NextMove) :- getNextAvailableBoards(State, NextBoardsPoss), head(NextBoardsPoss, UPos), getBoard(State, UPos, Board), nth0(Index, Board, ''), nth0(Index, [nw, n, ne, w, c, e, sw, s, se], Pos),NextMove = (UPos, Pos), !.

% dummy_last/2
% dummy_last(+State, -NextMove)
% Predicatul leagă NextMove la următoarea mutare pentru starea State.
% Strategia este foarte simplă: va fi aleasă cea mai din dreapta-jos mutare posibilă 
% (ultima din lista de poziții disponibile).

myReverse([],[]).
myReverse([H|T],RevList):- myReverse(T,RevT), myConcat(RevT,[H],RevList).

dummy_last(State, NextMove) :- getNextAvailableBoards(State, AvailBoards), lengthIsOne(AvailBoards), head(AvailBoards, UPos), getBoard(State, UPos, Board), myReverse(Board, BoardRev), nth0(Index, BoardRev, ''), nth0(Index, [se, s, sw, e, c, w, ne, n, nw], Pos),NextMove = Pos, !.
dummy_last(State, NextMove) :- getNextAvailableBoards(State, NextBoardsPoss), last(UPos, NextBoardsPoss), getBoard(State, UPos, Board), myReverse(Board, BoardRev), nth0(Index, BoardRev, ''), nth0(Index, [se, s, sw, e, c, w, ne, n, nw], Pos),NextMove = (UPos, Pos), !.

% ======== Etapa 2

% movePriority/4
% movePriority(+Player, +Board, +Move, -Priority)
% Calculează prioritatea mutării Move pentru jucătorul Player, într-o
% tablă individuală Board. Vezi enunț.

replace(I, L, E, K) :-
  nth0(I, L, _, R),
  nth0(I, K, E, R), !.

makeMoveOnBoard(Player, Board, Move, NewBoard) :- indexOf([nw, n, ne, w, c, e, sw, s, se], Move, Position), replace(Position, Board, Player, NewBoard), !.

emptyPositions(Board, EmptyPos) :- findall(X,(member(X, [nw, n, ne, w, c, e, sw, s, se]), indexOf([nw, n, ne, w, c, e, sw, s, se], X, Position), nth0(Position, Board, '')), EmptyPos).
checkFutureWin(Player, Board, EmptyPos, WinnerPos) :- findall(X, (member(X, EmptyPos), priority0(Player, Board, X)), WinnerPos).

getOtherPlayer(Player1, Player2) :- Player1 = x, Player2 = 0.
getOtherPlayer(Player1, Player2) :- Player1 = 0, Player2 = x.

priority0(Player, Board, Move) :- makeMoveOnBoard(Player, Board, Move, NewBoard), findall(Player,player_wins(Player,NewBoard),List), lengthIsOne(List), !.
priority1(Player, Board, Move) :- makeMoveOnBoard(Player2, Board, Move, NewBoard), getOtherPlayer(Player, Player2), findall(Player2,player_wins(Player2,NewBoard),List), lengthIsOne(List), !.
priority2(Player, Board, Move) :- empty_board(Board), member(Move, [nw, ne, sw, se]).
priority31(Player, Board, Move) :- member(Move, [nw, ne, sw, se]), \+ member(Player, Board), getOtherPlayer(Player, Player2), indexOf([nw, n, ne, w, c, e, sw, s, se], c, Position), nth0(Position, Board, Player2).
priority32(Player, Board, Move) :- Move = c, \+ member(Player, Board), getOtherPlayer(Player, Player2), indexOf([nw, n, ne, w, c, e, sw, s, se], c, Position), \+ nth0(Position, Board, Player2); Move = c, empty_board(Board).
priority4(Player, Board, Move) :- makeMoveOnBoard(Player, Board, Move, NewBoard), emptyPositions(Board, EmptyPos), checkFutureWin(Player, NewBoard, EmptyPos, WinnerPos), \+ lungime(WinnerPos, 0).

movePriority(Player, Board, Move, Priority) :- priority0(Player, Board, Move), Priority = 0, !;
priority1(Player, Board, Move), Priority = 1, !;
priority2(Player, Board, Move), Priority = 2, !;
priority31(Player, Board, Move), Priority = 3, !;
priority32(Player, Board, Move), Priority = 3, !;
priority4(Player, Board, Move), Priority = 4, !;
member(Move, [nw, ne, sw, se]), Priority = 5, !;
Priority = 6, !.

% bestIndividualMoves/3
% bestIndividualMoves(+P, +Board, -Moves)
% Leagă Moves la o listă cu toate mutările disponibile, în ordinea
% priorității lor.
%
% Hint: construiți o listă de perechi (prioritate, mutare) și folosiți
% sortMoves/2 pentru a obține lista de mutări, în ordinea priorității.

makePriorityPairs(Positions, Board, Player, PairsList) :- findall((Y, X), (member(X, Positions), movePriority(Player, Board, X, Y)), PairsList).

bestIndividualMoves(Player, Board, Moves) :- emptyPositions(Board, EmptyPos), makePriorityPairs(EmptyPos, Board, Player, PairsList), sortMoves(PairsList, Moves).

% narrowGreedy/2
% narrowGreedy(+State, -Move)
% Strategie care întotdeauna ia cea mai bună mutare individuală.
% Dacă sunt mai multe table disponibile, ia tabla care este cea mai bună
% mutare individuală în raport cu U-board.

narrowGreedy(State, Move) :- getNextAvailableBoards(State, NextBoard), lengthIsOne(NextBoard), head(NextBoard, BoardPos),
getBoard(State, BoardPos, Board), getNextPlayer(State, NextPlayer), bestIndividualMoves(NextPlayer, Board, Moves), head(Moves, Move).
narrowGreedy(State, (BoardPos, Move)) :- getNextAvailableBoards(State, NextBoards), getNextPlayer(State, NextPlayer), head(NextBoard, BoardPos),
getBoard(State, BoardPos, Board), getNextPlayer(State, NextPlayer), bestIndividualMoves(NextPlayer, Board, Moves), head(Moves, Move).

% bestMoves/2
% bestMoves(+State, -Moves)
% Leagă Moves la o listă care conține toate mutările disponibile, în
% ordinea priorității lor, după ordonarea prezentată în enunț.
bestMoves(_, _) :- false.

% greedy/2
% greedy(+State, -Move)
% Strategie care alege cea mai bună mutare, bazat pe rezultatul lui
% bestMoves/2.
greedy(_, _) :- false.
