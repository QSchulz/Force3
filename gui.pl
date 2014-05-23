:- module(mod_ui, [init_ui/0, game_ia/2, play/0, ask_id/1, start/1, welcome/0]).
:- use_module('jeu.pl').
:- use_module('regles.pl').
:- use_module('evaluation.pl').

% Stock l'état du plateau de jeu
:- dynamic board/1.

% Initialise l'interface utilisateur
init_ui:-
    retractall(board(_)),
    empty_board(P),
    assert(board(P)).

% Demande un coup à l'utilisateur
ask_placement(PL, [C1, C2, ID]) :-
    display_coords(PL), nl,
    writeln('Choix disponibles :'),
    writeln('\t0.\tPoser un pion'),
    writeln('\t1.\tDéplacer un pion'),
    writeln('\t2.\tDéplacer le taquin'),
    ask_id(ID),
    ID == 0 -> ID is 0, C1 is -1, ask_dest(C2);
    ID == 2 -> ask_orig(C1), ask_dest(C2), id_move(C2, C1, ID);
    ask_orig(C1), ask_dest(C2).

ask_id(ID) :-
    read(ID),
    integer(ID),
    between(0, 3, ID), !.
ask_id(ID) :-
    writeln('Choix invalide. Reprécisez.'),
    ask_id(ID).

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

% Récupère l'id du mouvement effectué
id_move(0, C2, 2) :-
    member(C2, [1, 3]).
id_move(1, C2, 2) :-
    member(C2, [0, 2, 4]).
id_move(2, C2, 2) :-
    member(C2, [1, 5]).
id_move(3, C2, 2) :-
    member(C2, [0, 4, 6]).
id_move(4, C2, 2) :-
    member(C2, [1, 5, 7, 3]).
id_move(5, C2, 2) :-
    member(C2, [2, 8, 4]).
id_move(6, C2, 2) :-
    member(C2, [3, 7]).
id_move(7, C2, 2) :-
    member(C2, [4, 8, 6]).
id_move(8, C2, 2) :-
    member(C2, [5, 7]).

id_move(0, C2, 3) :-
    member(C2, [6, 2]).
id_move(1, 7, 3).
id_move(2, C2, 3) :-
    member(C2, [0, 8]).
id_move(3, 5, 3).
id_move(5, 3, 3).
id_move(6, C2, 3) :-
    member(C2, [0, 8]).
id_move(7, 1, 3).
id_move(8, C2, 3) :-
    member(C2, [2, 6]).

getLevel(0, 'Facile').
getLevel(1, 'Moyen').
getLevel(2, 'Difficile').


% Sauvegarde le coup joué
save_play(Joueur,Coup) :-
    retract(board(B)),
    move(Joueur,B,Coup,B1),
    assert(board(B1)).

% IA vs. IA
game_ia(Level1, Level2, LevelIA1, LevelIA2, P1, P2) :-
    board(PL),
    alpha_beta(1, LevelIA1, PL, -200, 200, Coup, P1, _Valeur), !,
    save_play(1, Coup),
    board(NPL),
    display_board(1, Level1, NPL),
    not(won),
    alpha_beta(2, LevelIA2, NPL, -200, 200, Coup2, P2, _Valeur2), !,
    save_play(2, Coup2),
    board(NPL2),
    display_board(2, Level2, NPL2),
    not(won),
    game_ia(Level1, Level2, LevelIA1, LevelIA2, PL, NPL).

game_ia(Level1, Level2):-
    LevelIA1 is (Level1 + 1) * 2,
    LevelIA2 is (Level2 + 1) * 2,
    game_ia(Level1, Level2, LevelIA1, LevelIA2, [-1], [-1]).

% Fait jouer l'IA
play_ia(P1, Level) :-
    board(PL),
    Level1 is (Level + 1) * 2,
    alpha_beta(1, Level1, PL, -200, 200, Coup, P1, _Valeur), !,
    save_play(1, Coup),
    board(NPL),
    display_board(1, Level, NPL),
    not(won),
    play(PL, Level).

% Demande au joueur de jouer
play(LastBoard, Level) :-
    board(PL),
    ask_placement(PL, Coup),
    save_play(2, Coup),
    board(NPL),
    display_board(2, -1, NPL),
    not(won),
    play_ia(LastBoard, Level).

play :-
    writeln('Niveau (IA) :'),
    writeln('\t0.\tFacile'),
    writeln('\t1.\tMoyen'),
    writeln('\t2.\tDifficile'),
    ask_id(Level),
    empty_board(Board),
    play(Board, Level).

% Vérifie si le joueur a gagné et propose de recommencer.
won :-
    board(PL),
    win(JR, PL),
    writef("Le joueur %w a gagné !", [JR]),
    init_ui, !.

% Affiche le plateau de jeu
display_board(J, IDLevel, [C1, C2, C3, C4, C5, C6, C7, C8, C9]):-
    IDLevel \= -1, !,
    getLevel(IDLevel, Level),
    write('     _____'), nl,
    write('    |'), afc(C1), write(' '), afc(C2), write(' '), afc(C3), write('|'), nl,
    write(' '), write(J),
    write('  |'), afc(C4), write(' '), afc(C5), write(' '), afc(C6), write('|'),
    write('\tNiveau (IA) '), write(Level), nl,
    write('    |'), afc(C7), write(' '), afc(C8), write(' '), afc(C9), write('|'), nl,
    write('     -----'), nl, nl.

display_board(J, -1, [C1, C2, C3, C4, C5, C6, C7, C8, C9]):-
    !,
    write('     _____'), nl,
    write('    |'), afc(C1), write(' '), afc(C2), write(' '), afc(C3), write('|'), nl,
    write(' '), write(J),
    write('  |'), afc(C4), write(' '), afc(C5), write(' '), afc(C6), write('|\tJoueur'), nl,
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
    writeln('|\tQuentin Schulz          |'),
    writeln('------------------------------'),
    nl.

% Menu pour la difficulté des IA
level_ia :-
    menu_ia(1, Level1),
    menu_ia(2, Level2),
    tty_clear,
    game_ia(Level1, Level2).

menu_ia(ID, Level) :-
    write('-----Niveau IA '), write(ID), writeln('--------------'),
    writeln('|  0.\tFacile               |'),
    writeln('|  1.\tMoyen                |'),
    writeln('|  2.\tDifficile            |'),
    writeln('------------------------------'),
    ask_id(Level).

% Lance en fonction du choix du joueur
start(0) :-
    play.
start(1) :-
    level_ia.
start(_) :- !.
