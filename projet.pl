adversaire(1,2).
adversaire(2,1).

init_game([1, 0, 1,
	   0,-1, 0,
	   2, 0, 2]).
	   
jouer(X):-init_game(L), eval_board(L, 2, X).

%#########################
%Déclaration des prédicats dynamiques
%	-prev_move_taquin est mis à jour à chaque tour à partir de la valeur du dernier déplacement de taquin
%	-best_next_move est mis à jour à chaque node du negamax
%#########################

:- dynamic prev_move_taquin/2.
:- dynamic best_next_move/1.

%########################
%Evaluation de la proposition (classement suivant l efficacité)
%########################
%	- Valeur retourne 100 (- le nombre de tours nécessaires si Joueur a gagné ?)
%	- Valeur retourne -100 (+ le nombre de tours nécessaires si Joueur a perdu ?)
% 	- Valeur retourne 0 si le jeu termine à égalité.
%	- Si le jeu n est pas terminé, on attribue un score à l état actuel

eval_board(Jeu, Joueur, Valeur):-
	partie_finie(Jeu, X),	
	X = Joueur,
	!,
	Valeur is 100. %à voir nbTours = profondeur
	
eval_board(Jeu, Joueur, Valeur):-
	partie_finie(Jeu, X),
	adversaire(Joueur, X),
	!,
	Valeur is -100 .%à voir nbTours
	
eval_board(Jeu, _, 0):-
	partie_finie(Jeu, 0),
	!. %à voir pour le nombre de tours

eval_board([C0, C1, C2, C3, C4, C5, C6, C7, C8], Joueur, Valeur):-
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
	Valeur is X1 + X2 + X3 + X4 + X5 + X6 + X7 + X8 - Y1 - Y2 - Y3 - Y4 - Y5 - Y6 - Y7 - Y8.

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
	!.

partie_finie([X, X, X, JR, JR, JR, _, _, _], 0):-
	adversaire(JR, X),
	!.
	
partie_finie([_, _, _, JR, JR, JR, X, X, X], 0):-
	adversaire(JR, X),
	!.
	
partie_finie([_, _, _, JR, JR, JR|_], JR):-
	!.

partie_finie([X, X, X, _, _, _, JR, JR, JR], 0):-
	adversaire(JR, X),
	!.
	
partie_finie([_, _, _, X, X, X, JR, JR, JR], 0):-
	adversaire(JR, X),
	!.
	
partie_finie([_, _, _, _, _, _, JR, JR, JR|_], JR):-
	!.
	
%Détection des colonnes
partie_finie([JR, X, _, JR, X, _, JR, X, _], 0):-adversaire(JR, X),!.
partie_finie([JR, _, X, JR, _, X, JR, _, X], 0):-adversaire(JR, X),!.
partie_finie([JR, _, _, JR, _, _, JR, _, _], JR):-!.

partie_finie([X, JR, _, X, JR, _, X, JR, _], 0):-adversaire(JR, X),!.
partie_finie([_, JR, X, _, JR, X, _, JR, X], 0):-adversaire(JR, X),!.
partie_finie([_, JR, _, _, JR, _, _, JR, _], JR):-!.

partie_finie([X, _, JR, X, _, JR, X, _, JR], 0):-adversaire(JR, X),!.
partie_finie([_, X, JR, _, X, JR, _, X, JR], 0):-adversaire(JR, X),!.
partie_finie([_, _, JR, _, _, JR, _, _, JR], JR):-!.
%Détection des diagonales
partie_finie([_, _, JR, _, JR, _, JR, _, _], JR):-!.

partie_finie([JR, _, _, _, JR, _, _, _, JR], JR):-!.

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
count(_, [], 0) :-
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
	taquin_(CD, CA),
	!.
	
taquin(CD, CA):-
	neighbour(CD, CA).

%#########################
%Possibilité d'une pose d'un pion du joueur JR sur la case CA
%#########################
can_move(JR, p, Jeu, _, CA) :-
	nth0(CA, Jeu, 0),
	count(JR, Jeu, X),
	X < 3.

%#########################
%Possibilité du déplacement d'un pion du joueur JR de la case CD vers la case CA
%#########################
can_move(JR, d, Jeu, CD, CA) :- 
	nth0(CD, Jeu, JR),
	nth0(CA, Jeu, 0),
	neighbour(CD, CA).

%#########################
%Possibilité du déplacement du taquin de la case CD à la case CA
%#########################
can_move(_, t, Jeu, CD, CA) :-
	nth0(CD, Jeu, -1),
	taquin(CD, CA),
	\+prev_move_taquin(CA, CD).
	
%#########################
%Trouver l'ensemble des possibilités de mouvements suivant le type de jeu T.
%#########################

move(JR, T, Jeu, Next):-
	can_move(JR, T, Jeu, CD, CA),
	fill(JR, T, Jeu, Next, CD, CA).
	
%#########################
%Interchanger la valeur X et Y des cases d'index CD et CA d'une matrice Jeu vers une matrice Next.
%#########################

swap(_,_,[],[],_,_,_,_,_).

swap(_,_,Jeu,Jeu,_,_,CD,CA,I):-
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

fill(JR, p, Jeu, Next, CD, CA):-
	swap(JR, p, Jeu, Next, JR, -2, -1, CA, 0),
	!.
	
fill(JR, d, Jeu, Next, CD, CA):-
	nth0(CA, Jeu, Y),
	nth0(CD, Jeu, X), 
	swap(JR, d, Jeu, Next, X, Y, CD, CA, 0),
	!.
	
fill(JR, t, Jeu, Next, CD, CA):-
	neighbour(CD, CA),
	!,
	nth0(CA, Jeu, Y),
	nth0(CD, Jeu, X),
	write(X),
	writeln(CD),
	write(Y),
	writeln(CA),
	swap(JR, t, Jeu, Next, X, Y, CD, CA, 0),
	!.
	
fill(JR, t, Jeu, Next, CD, CA):-
	neighbour(CD, CT),
	neighbour(CT, CA),
	nth0(CT, Jeu, Z),
	nth0(CA, Jeu, Y),
	nth0(CD, Jeu, X),
	write(X),
	writeln(CD),
	write(Y),
	writeln(CA),
	swap(JR, t, Jeu, Next1, X, Z, CD, CT, 0),
	swap(JR, t, Next1, Next, Z, Y, CT, CA, 0),
	!.

%#########################
%Dessiner la matrice de jeu sur trois lignes.
%#########################
	
draw([C0, C1, C2, C3, C4, C5, C6, C7, C8]):-
	writeln([C0, C1, C2]),
	writeln([C3, C4, C5]),
	writeln([C6, C7, C8]),
	writeln('-----------').

%#########################
%Recherche de tous les coups possibles
%#########################

findnextmoves(Jeu, Moves, JR):-
	writeln('Matrice de jeu de départ'),
	draw(Jeu),
	findall(Move, move(JR, d, Jeu, Move), List),
	writeln('Déplacements pion:'),
	foreach(member(X, List), draw(X)),
	findall(Move2, move(JR, p, Jeu, Move2), List2),
	writeln('Poses:'),
	foreach(member(X, List2), draw(X)),
	findall(Move3, move(JR, t, Jeu, Move3), List3),
	writeln('Déplacements taquin:'),
	foreach(member(X, List3), draw(X)),
	append([List,List2,List3], Moves).

%#########################
%Algorithme du negamax
%BV is -100000 est l'équivalent pour l'infini (on ne dépassera normalement jamais les 100).
%#########################
negamax(Jeu, 0, _,_,JR, X):-
	eval_board(Jeu, JR, X),
	!,
	write('profondeur atteinte').
	
negamax(Jeu, _,_,_,JR, X):-
	partie_finie(Jeu, _),
	!,
	eval_board(Jeu, JR, X),
	write('la win').
	
negamax(Jeu, Prof, Alpha, Beta, JR, BestVal):-
	BV is -100000,
	findnextmoves(Jeu, Moves, JR),
	for_each(Moves, Prof, Alpha, Beta, JR, BestVal, BV).%, best_next_move(X), write(X), update_prev_move_taquin(Jeu, X).%, draw(X), adversaire(JR, Y), newturn(Y).

%#########################
%On itère sur l'ensemble des mouvements possibles et on trouve la meilleure valeur.
%#########################
for_each([], _, _, _, _, BV, BV).

for_each([Jeu|Moves], Prof, Alpha, Beta, JR, BestVal, BV):-
	Prof1 is Prof-1,
	Alpha1 is -Alpha,
	Beta1 is -Beta,
	adversaire(JR, Y),
	negamax(Jeu, Prof1, Beta1, Alpha1, Y, X),
	Val is -X,
	BestVal is max(BV, Val),
	BestVal = Val,
	retract(best_next_move(_,_)),
	assert(best_next_move(Jeu)),
	Alpha2 is max(Alpha, Val),
	Alpha2 >= Beta,
	!.

for_each([Jeu|Moves], Prof, Alpha, Beta, JR, BestVal, BV):-
	Prof1 is Prof-1,
	Alpha1 is -Alpha,
	Beta1 is -Beta,
	adversaire(JR, Y),
	negamax(Jeu, Prof1, Beta1, Alpha1, Y, X),
	Val is -X,
	BV1 is max(BV, Val),
	BV1 = Val,
	retract(best_next_move(_,_)),
	assert(best_next_move(Jeu)),
	Alpha2 is max(Alpha, Val),
	Alpha2 < Beta,
	for_each(Moves, Prof, Alpha2, Beta, JR, BestVal, BV1).

for_each([Jeu|Moves], Prof, Alpha, Beta, JR, BestVal, BV):-
	Prof1 is Prof-1,
	Alpha1 is -Alpha,
	Beta1 is -Beta,
	adversaire(JR, Y),
	negamax(Jeu, Prof1, Beta1, Alpha1, Y, X),
	Val is -X,
	BV1 is max(BV, Val),
	BV1 = BV,
	Alpha2 is max(Alpha, Val),
	Alpha2 < Beta,
	for_each(Moves, Prof, Alpha2, Beta, JR, BestVal, BV1).

%#########################
%Mettre à jour la valeur du dernier mouvement du taquin (s'il reste sur place: création quand même).
%#########################

update_prev_move_taquin(Jeu, Next):-
	nth0(CD, Jeu, -1),
	nth0(CA, Next, -1),
	retract(prev_move_taquin(_,_))
	assert(prev_move_taquin(CD, CA)).

