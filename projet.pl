:-module(projet, [adversaire/2, init_game/1, play_human/4, play_ia/4, play_ia2/5, can_move/5, fill/6]). 
:-use_module('gui.pl').


adversaire(1,2).
adversaire(2,1).

init_game([0, 0, 0,
	   0,-1, 0,
	   0, 0, 0]).

play_human(Jeu, Last, Diff, JR):-
	\+partie_finie(Jeu, _Gagnant),
	ask_placement(JR, Jeu, CD, CA, ID, Last),
	fill(JR, ID, Jeu, Coup, CD, CA),
	display_board(JR, -1, Coup),
	adversaire(JR, JR2),
	play_ia(Coup, Jeu, Diff, JR2),
	!.

play_human(Jeu, _Last, _Diff, _JR):-
	partie_finie(Jeu, 0),
	write('Egalité!'),
	!.
	
play_human(Jeu, _Last, _Diff, _JR):-
	partie_finie(Jeu, X),
	writef('Joueur %w gagne!', [X]),
	!.
	
play_ia(Jeu, Last, Diff, JR):-
	\+partie_finie(Jeu, _Gagnat),
	negamax(Jeu, Diff, JR, _BestValue, Coup, [Last, Jeu], Diff),
	display_board(JR, 1, Coup),
	adversaire(JR, JR2),
	play_human(Coup, Jeu, Diff, JR2),
	!.

play_ia(Jeu, _Last, _Diff, _JR):-
	partie_finie(Jeu, 0),
	write('Egalité!'),
	!.
	
play_ia(Jeu, _Last, _Diff, _JR):-
	partie_finie(Jeu, X),
	writef('Joueur %w gagne!', [X]),
	!.
	
play_ia2(Jeu, Last, Diff, Diff2, JR):-
	\+partie_finie(Jeu, _Gagnant),
	negamax(Jeu, Diff, JR, _BestValue, Coup, [Last, Jeu], Diff),
	display_board(JR, 1, Coup),
	adversaire(JR, JR2),
	play_ia2(Coup, Jeu, Diff2, Diff, JR2),
	!.

play_ia2(Jeu, _Last, _Diff, _Diff2, _JR):-
	partie_finie(Jeu, 0),
	write('Egalité!'),
	!.
	
play_ia2(Jeu, _Last, _Diff, _Diff2, _JR):-
	partie_finie(Jeu, X),
	writef('IA %w gagne!', [X]),
	!.

%########################
%Evaluation de la proposition (classement suivant l efficacité)
%########################
%	- Valeur retourne 100 (- le nombre de tours nécessaires si Joueur a gagné ?)
%	- Valeur retourne -100 (+ le nombre de tours nécessaires si Joueur a perdu ?)
% 	- Valeur retourne 0 si le jeu termine à égalité.
%	- Si le jeu n est pas terminé, on attribue un score à l état actuel

eval_board(Jeu, Joueur, Valeur, Level, Depth):-
	partie_finie(Jeu, X),	
	X = Joueur,
	!,
	Valeur is 100-(Level-Depth). %à voir nbTours = profondeur
	
eval_board(Jeu, Joueur, Valeur, Level, Depth):-
	partie_finie(Jeu, X),
	adversaire(Joueur, X),
	!,
	Valeur is -100+(Level-Depth).%à voir nbTours
	
eval_board(Jeu, _Joueur, 0, _Level, _Depth):-
	partie_finie(Jeu, 0),
	!. %à voir pour le nombre de tours

eval_board([C0, C1, C2, C3, C4, C5, C6, C7, C8], Joueur, Valeur, Level, Depth):-
	point(Joueur, [C0, C1, C2], X1),
	point(Joueur, [C3, C4, C5], X2),
	point(Joueur, [C6, C7, C8], X3),
	point(Joueur, [C0, C3, C6], X4),
	point(Joueur, [C1, C4, C7], X5),
	point(Joueur, [C2, C5, C8], X6),
	point(Joueur, [C0, C4, C8], X7),
	point(Joueur, [C2, C4, C6], X8),
	adversaire(Joueur, Joueur2),
	point(Joueur2, [C0, C1, C2], Y1),
	point(Joueur2, [C3, C4, C5], Y2),
	point(Joueur2, [C6, C7, C8], Y3),
	point(Joueur2, [C0, C3, C6], Y4),
	point(Joueur2, [C1, C4, C7], Y5),
	point(Joueur2, [C2, C5, C8], Y6),
	point(Joueur2, [C0, C4, C8], Y7),
	point(Joueur2, [C2, C4, C6], Y8),
	Valeur is X1 + X2 + X3 + X4 + X5 + X6 + X7 + X8 - Y1 - Y2 - Y3 - Y4 - Y5 - Y6 - Y7 - Y8 - (Level-Depth).

%#########################
%Détection de fin du jeu
%#########################
%	- X retourne 0 s il y a égalité (les deux joueurs ont réussi à aligner trois pions)
%	- X retourne 1 si Joueur1 a aligné ses trois pions
%	- X retourne 2 si Joueur2 a aligné ses trois pions
% 	Le prédicat échoue si aucun des joueurs n a aligné trois pions

%Détection des lignes
partie_finie([JR, JR, JR, X, X, X, _, _, _], 0):-
	adversaire(JR, X),
	!.
	
partie_finie([JR, JR, JR, _, _, _, X, X, X], 0):-
	adversaire(JR, X),
	!.
	
partie_finie([JR, JR, JR|_], JR):-
	JR \=0 ,
	!.

partie_finie([X, X, X, JR, JR, JR, _, _, _], 0):-
	adversaire(JR, X),
	!.
	
partie_finie([_, _, _, JR, JR, JR, X, X, X], 0):-
	adversaire(JR, X),
	!.
	
partie_finie([_, _, _, JR, JR, JR|_], JR):-
	JR \= 0,
	!.

partie_finie([X, X, X, _, _, _, JR, JR, JR], 0):-
	adversaire(JR, X),
	!.
	
partie_finie([_, _, _, X, X, X, JR, JR, JR], 0):-
	adversaire(JR, X),
	!.
	
partie_finie([_, _, _, _, _, _, JR, JR, JR|_], JR):-
	JR \= 0,
	!.
	
%Détection des colonnes
partie_finie([JR, X, _, JR, X, _, JR, X, _], 0):-adversaire(JR, X),!.
partie_finie([JR, _, X, JR, _, X, JR, _, X], 0):-adversaire(JR, X),!.
partie_finie([JR, _, _, JR, _, _, JR, _, _], JR):- JR \= 0, !.

partie_finie([X, JR, _, X, JR, _, X, JR, _], 0):-adversaire(JR, X),!.
partie_finie([_, JR, X, _, JR, X, _, JR, X], 0):-adversaire(JR, X),!.
partie_finie([_, JR, _, _, JR, _, _, JR, _], JR):- JR \= 0, !.

partie_finie([X, _, JR, X, _, JR, X, _, JR], 0):-adversaire(JR, X),!.
partie_finie([_, X, JR, _, X, JR, _, X, JR], 0):-adversaire(JR, X),!.
partie_finie([_, _, JR, _, _, JR, _, _, JR], JR):- JR \=0, !.
%Détection des diagonales
partie_finie([_, _, JR, _, JR, _, JR, _, _], JR):- JR \= 0, !.

partie_finie([JR, _, _, _, JR, _, _, _, JR], JR):-JR\=0, !.

%#########################
%Attribution des points si la partie n'est pas terminée
%#########################
%	- 5 points pour 2 pions sur la même ligne/colonne/diagonale
%	- 1 point pour 1 pion sur la même ligne/colonne/diagonale
%	- 0 sinon.

point(List, Joueur, 5):-
	count(List, Joueur, 2),
	!.
	
point(List, Joueur, 1):-
	count(List, Joueur, 1),
	!.
	
point(List, Joueur, 0):-
	count(List, Joueur, 0).

%#########################
%Récupération du nombre de pions posés par le joueur JR
%#########################
count(_JR, [], 0) :-
	!.
	
count(JR, [JR|R], I) :-
	count(JR, R, I2),
	I is I2 + 1.
	
count(JR, [Y|R], I) :-
	JR \= Y,
	count(JR, R, I).

%#########################
%Définition des voisins
%#########################
neighbour(0,1).
neighbour(0,3).
neighbour(1,0).
neighbour(1,2).
neighbour(1,4).
neighbour(2,1).
neighbour(2,5).
neighbour(3,0).
neighbour(3,4).
neighbour(3,6).
neighbour(4,1).
neighbour(4,3).
neighbour(4,5).
neighbour(4,7).
neighbour(5,2).
neighbour(5,4).
neighbour(5,8).
neighbour(6,3).
neighbour(6,7).
neighbour(7,4).
neighbour(7,6).
neighbour(7,8).
neighbour(8,5).
neighbour(8,7).

%#########################
%Définition des possibles mouvements du taquin
%#########################
taquin_(0,2).
taquin_(0,6).
taquin_(1,7).
taquin_(2,0).
taquin_(2,8).
taquin_(3,5).
taquin_(5,3).
taquin_(6,0).
taquin_(6,8).
taquin_(7,1).
taquin_(8,2).
taquin_(8,6).
taquin(CD, CA):-
	taquin_(CD, CA).
	
taquin(CD, CA):-
	neighbour(CD, CA).

%#########################
%Possibilité d'une pose d'un pion du joueur JR sur la case CA
%#########################
can_move(JR, 0, Jeu, _CD, CA) :-
	nth0(CA, Jeu, 0),
	count(JR, Jeu, X),
	X < 3.

%#########################
%Possibilité du déplacement d'un pion du joueur JR de la case CD vers la case CA
%#########################
can_move(JR, 1, Jeu, CD, CA) :- 
	nth0(CD, Jeu, JR),
	nth0(CA, Jeu, 0),
	neighbour(CD, CA).

%#########################
%Possibilité du déplacement du taquin de la case CD à la case CA
%#########################
can_move(_JR, 2, Jeu, CD, CA) :-
	nth0(CD, Jeu, -1),
	taquin(CD, CA).
	
%#########################
%Trouver l'ensemble des possibilités de mouvements suivant le type de jeu T.
%#########################

move(JR, T, Jeu, Next):-
	can_move(JR, T, Jeu, CD, CA),
	fill(JR, T, Jeu, Next, CD, CA).
	
%#########################
%Interchanger la valeur X et Y des cases d'index CD et CA d'une matrice Jeu vers une matrice Next.
%#########################

swap(_JR,_T,[],[],_X,_Y,_CD,_CA,_I).

swap(_JR,_T,Jeu,Jeu,_X,_Y,CD,CA,I):-
	I > CD,
	I > CA,
	!.
	
swap(JR, T, [A|Jeu], [A|Next], X, Y, CD, CA, I):-
	I \= CD,
	I \= CA,
	I1 is I+1,
	swap(JR, T, Jeu, Next, X, Y, CD, CA, I1),
	!.
	
swap(JR, T, [_|Jeu], [X|Next], X, Y, CD, CA, I):-
	I = CA,
	I1 is I+1,
	swap(JR, T, Jeu, Next, X, Y, CD, CA, I1),
	!.
	
swap(JR, T, [_|Jeu], [Y|Next], X, Y, CD, CA, I):-
	I = CD,
	I1 is I+1,
	swap(JR, T, Jeu, Next, X, Y, CD, CA, I1),
	!.
	
%#########################
%Remplir la nouvelle matrice de jeu Next avec l'ordre de jeu T allant de CD à CA depuis la matrice Jeu.
%#########################

fill(JR, 0, Jeu, Next, _CD, CA):-
	swap(JR, 0, Jeu, Next, JR, -2, -1, CA, 0),
	!.
	
fill(JR, 1, Jeu, Next, CD, CA):-
	nth0(CA, Jeu, Y),
	nth0(CD, Jeu, X), 
	swap(JR, 1, Jeu, Next, X, Y, CD, CA, 0),
	!.
	
fill(JR, 2, Jeu, Next, CD, CA):-
	neighbour(CD, CA),
	!,
	nth0(CA, Jeu, Y),
	nth0(CD, Jeu, X),
	swap(JR, 2, Jeu, Next, X, Y, CD, CA, 0),
	!.
	
fill(JR, 2, Jeu, Next, CD, CA):-
	neighbour(CD, CT),
	neighbour(CT, CA),
	nth0(CT, Jeu, Z),
	nth0(CA, Jeu, Y),
	nth0(CD, Jeu, X),
	swap(JR, 2, Jeu, Next1, X, Z, CD, CT, 0),
	swap(JR, 2, Next1, Next, X, Y, CT, CA, 0),
	!.

%#########################
%Recherche de tous les coups possibles
%#########################

findnextmoves(Jeu, Moves, JR, ForbiddenMove):-
	findall(Move, move(JR, 0, Jeu, Move), List),
	findall(Move2, move(JR, 1, Jeu, Move2), List2),
	findall(Move3, move(JR, 2, Jeu, Move3), List3),
	append([List,List2,List3], List4),
	delete(List4, ForbiddenMove, Moves).

%#########################
%Algorithme du negamax
%#########################
negamax(Jeu, 0, JR, X, Jeu, _ForbiddenMove, Level):-
	!,
	eval_board(Jeu, JR, X, Level, 0).
	
negamax(Jeu, Depth, JR, X, Jeu, _ForbiddenMove, Level):-
	partie_finie(Jeu, _Gagnant),
	!,
	eval_board(Jeu, JR, X, Level, Depth).
	
negamax(Jeu, Prof, JR, BestVal, BestMove, [ForbiddenMove, NextForbiddenMove], Level):-
	findnextmoves(Jeu, Moves, JR, ForbiddenMove),
	for_each(Moves, Prof, JR, -1000, BestVal, BestMove, [ForbiddenMove, NextForbiddenMove], Level).

%#########################
%On itère sur l'ensemble des mouvements possibles et on trouve la meilleure valeur.
%#########################
%Les deux prédicats suivants sont les appels finaux (quand il ne reste qu'une node dans la même profondeur).
for_each([Move], Prof, JR, Val, BestVal, Move, [_ForbiddenMove, NextForbiddenMove], Level):-
	Prof1 is Prof-1,
	adversaire(JR, JR2),
	negamax(Move, Prof1, JR2, Val1, _BestMove, [NextForbiddenMove, Move], Level),
	Val2 is -Val1,
	BestVal is max(Val2, Val),
	BestVal = Val2,
	!.
	
for_each([Move], Prof, JR, Val, BestVal, _Move, [_ForbiddenMove, NextForbiddenMove], Level):-
	Prof1 is Prof-1,
	adversaire(JR, JR2),
	negamax(Move, Prof1, JR2, Val1, _MoveNegamax, [NextForbiddenMove, Move], Level),
	Val2 is -Val1,
	BestVal is max(Val2, Val),
	BestVal \= Val2,
	!.
	

for_each([Move|Moves], Prof, JR,Val, BestVal, Move, [ForbiddenMove, NextForbiddenMove], Level):-
	Prof1 is Prof-1,
	adversaire(JR, JR2),
	negamax(Move, Prof1, JR2, Val1, _BestMove, [NextForbiddenMove, Move], Level),
	Val2 is -Val1,
	Val3 is max(Val2, Val),
	for_each(Moves, Prof, JR, Val3, BestVal, _Move, [ForbiddenMove, NextForbiddenMove], Level),
	BestVal = Val3,
	!. 
	
for_each([Move|Moves], Prof, JR, Val, BestVal, BestMove, [ForbiddenMove, NextForbiddenMove], Level):-
	Prof1 is Prof-1,
	adversaire(JR, JR2),
	negamax(Move, Prof1, JR2, Val1, _BestMove, [NextForbiddenMove, Move], Level),
	Val2 is -Val1,
	Val3 is max(Val2, Val),
	for_each(Moves, Prof, JR, Val3, BestVal, BestMove, [ForbiddenMove, NextForbiddenMove], Level),
	BestVal \= Val3,!. 

