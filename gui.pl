:- module(mod_ui, [play/0, ask_menu/1, start/1, welcome/0, display_board/3, display_coords/1, ask_placement/5]).
:- use_module('projet.pl').



ask_orig(C) :-
    write('Coordonnée (origine) : '),
    read(C),
    integer(C),
    between(0, 8, C), !.
    
ask_orig(C) :-
    writeln('Coordonnée invalide. Reprécisez.'),
    ask_orig(C).

ask_dest(C) :-
    write('Coordonnée (destination) : '),
    read(C),
    integer(C),
    between(0, 8, C), !.
    
ask_dest(C) :-
    writeln('Mouvement invalide. Reprécisez.'),
    ask_dest(C).

% Demande un coup à l'utilisateur
ask_placement(JR, Jeu, CD, CA, Type) :-
    display_coords(Jeu), nl,
    writeln('Choix disponibles :'),
    writeln('\t0.\tPoser un pion'),
    writeln('\t1.\tDéplacer un pion'),
    writeln('\t2.\tDéplacer le taquin'),
    writeln('\t3.\tQuitter le jeu'),
    ask_jeu(Type),
    (Type == 0 ->
    	CD is -1, ask_dest(CA), can_move(JR, Type, Jeu, CD, CA); 	
     Type == 2 ->
    	ask_dest(CA), can_move(JR, Type, Jeu, CD, CA);
     Type == 1 ->
     	ask_orig(CD), ask_dest(CA), can_move(JR, Type, Jeu, CD, CA);
     halt).
    
ask_placement(JR, Jeu, CD, CA, Type) :-
	writeln('Choix impossible. Veuillez réessayer.'), ask_placement(JR, Jeu, CD, CA, Type).	
		  	

ask_menu(ID) :-
    read(ID),
    integer(ID),
    between(0, 3, ID), !.

ask_menu(ID) :-
    writeln('Choix invalide. Reprécisez.'),
    ask_menu(ID).    
    
ask_jeu(ID) :-
    read(ID),
    integer(ID),
    between(0, 4, ID), !.

ask_jeu(ID) :-
    writeln('Choix invalide. Reprécisez.'),
    ask_menu(ID).    

/*Jouer contre l'IA. L'utilisateur joue en premier.*/
play :-
    writeln('Niveau (IA) :'),
    writeln('\t0.\tFacile'),
    writeln('\t1.\tMoyen'),
    writeln('\t2.\tDifficile'),
    ask_menu(Level1),
    ia_level(Level1, Level),
    init_game(Board),
    play_human(Board, [0,0,0,0,0,0,0,0,0], Level, 1).

% Affiche le plateau de jeu
display_board(J, -1, [C1, C2, C3, C4, C5, C6, C7, C8, C9]):-
    !,
    write('     _____'), nl,
    write('    |'), afc(C1), write(' '), afc(C2), write(' '), afc(C3), write('|'), nl,
    write('  '),
    write('  |'), afc(C4), write(' '), afc(C5), write(' '), afc(C6), write('|\tJoueur '), write(J), nl,
    write('    |'), afc(C7), write(' '), afc(C8), write(' '), afc(C9), write('|'), nl,
    write('     -----'), nl, nl.

display_board(J, IDLevel, [C1, C2, C3, C4, C5, C6, C7, C8, C9]):-
    write('     _____'), nl,
    write('    |'), afc(C1), write(' '), afc(C2), write(' '), afc(C3), write('|'), nl,
    write('  '),
    write('  |'), afc(C4), write(' '), afc(C5), write(' '), afc(C6), write('| IA '), write(J), nl,
    write('    |'), afc(C7), write(' '), afc(C8), write(' '), afc(C9), write('|'), nl,
    write('     -----'), nl, nl.

% Affiche les coordonnées du plateau
display_coords([C1, C2, C3, C4, C5, C6, C7, C8, C9]):-
    write('     _____                           _____'), nl,
    write('    |'), afc(C1), write(' '), afc(C2), write(' '), afc(C3), write('|'),
    write('                         |0 1 2|'),nl,
    write('    |'), afc(C4), write(' '), afc(C5), write(' '), afc(C6), write('|'),
    write('     Coordonnees     --> |3 4 5|'), nl,
    write('    |'), afc(C7), write(' '), afc(C8), write(' '), afc(C9), write('|'),
    write('                         |6 7 8|'), nl,
    write('     -----                           -----'), nl, !.

% Affecte les cases en fonction de leur nature
afc(0) :-
    write(' ').
afc(-1) :-
    write('#').
afc(1) :-
    write('o').
afc(2):-
    write('x').

% Welcome prompt
welcome :-
    nl,
    writeln('Projet Force 3'),
    writeln('Créé dans le cadre du cours IA41 (UTBM)'), nl,
    writeln('-----Auteurs------------------'),
    writeln('|\tSimon Magnin-Feysot  |'),
    writeln('|\tQuentin Schulz       |'),
    writeln('------------------------------'),
    nl.

% Menu pour la difficulté des IA
level_ia :-
    menu_ia(1, Level1),
    menu_ia(2, Level2),
    init_game(Jeu),
    tty_clear,
    play_ia2(Jeu, [0,0,0,0,-1,0,0,0,0], Level1, Level2, 1).

menu_ia(ID, Level) :-
    write('-----Niveau IA '), write(ID), writeln('--------------'),
    writeln('|  0.\tFacile               |'),
    writeln('|  1.\tMoyen                |'),
    writeln('|  2.\tDifficile            |'),
    writeln('------------------------------'),
    ask_menu(Level1),
    ia_level(Level1, Level).
    
ia_level(Level1, Level):-Level is Level1+1.

% Lance en fonction du choix du joueur
start(0) :-
    play.
start(1) :-
    level_ia.
start(_) :- !.
