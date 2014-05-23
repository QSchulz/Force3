:- use_module('gui.pl').

% Affiche le menu
main :-
    welcome,
    init_ui,
    writeln('-----Menu---------------------'),
    writeln('|  0.\tJouer (J vs. IA)     |'),
    writeln('|  1.\tObserver (IA VS. IA) |'),
    writeln('|............................|'),
    writeln('|  2.\tQuitter              |'),
    writeln('------------------------------'),
    ask_id(Choix),
    tty_clear,
    start(Choix).


% Lance le menu
:- set_prolog_stack(global, limit(2*10**9)), main.
