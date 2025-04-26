%%% RESUME SELECTION SYSTEM IN PROLOG %%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% 1. DATABASE - JOBS AND CANDIDATES %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- consult('candidates.pl').
:- consult('jobs.pl').
:- use_module(filter).
:- use_module(score).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% 2. SYSTEM INITIALIZATION %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- initialization(system_start).

system_start :-
    write('Resume Selection System loaded.'), nl,
    write('Type "menu." to start the interactive interface.'), nl.

menu :-
    nl,
    write('=== RESUME SELECTION SYSTEM ==='), nl,
    write('1. List available jobs'), nl,
    write('2. List registered candidates'), nl,
    write('3. View qualified candidates for a job'), nl,
    write('4. View best candidates for a job'), nl,
    write('0. Exit'), nl,
    write('Option: '),
    read(Option),
    handle_option(Option).

handle_option(0) :- !, write('Exiting system...'), nl.
handle_option(1) :- list_jobs, menu.
handle_option(2) :- list_candidates, menu.
handle_option(3) :- consult_qualified, menu.
handle_option(4) :- consult_best, menu.
handle_option(_) :- write('Invalid option!'), nl, menu.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% 3. LISTING FUNCTIONS %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

list_jobs :-
    write('=== AVAILABLE JOBS ==='), nl,
    forall(job(ID, Title, Area, TargetCourse, Min_CRA, Min_Semester, Required, Desired, Min_Exp),
           (write('ID: '), write(ID), write(' - '), write(Title), nl,
            write('  Area: '), write(Area), nl,
            write('  Target Course: '), write(TargetCourse), nl,
            write('  Minimum CRA: '), write(Min_CRA), nl,
            write('  Minimum Semester: '), write(Min_Semester), nl,
            write('  Required Skills: '), write(Required), nl,
            write('  Desired Skills: '), write(Desired), nl,
            write('  Minimum Experience: '), write(Min_Exp), write(' years'), nl, nl)).

list_candidates :-
    write('=== REGISTERED CANDIDATES ==='), nl,
    forall(candidate(ID, Name, CRA, Semester, Course, ProExpYears, Techs, Interests),
           (write('ID: '), write(ID), write(' - '), write(Name), nl,
            write('  CRA: '), write(CRA), nl,
            write('  Semester: '), write(Semester), nl,
            write('  Course: '), write(Course), nl,
            write('  Professional Experience: '), write(ProExpYears), write(' years'), nl,
            write('  Technologies: '), write(Techs), nl,
            write('  Interest Areas: '), write(Interests), nl, nl)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% 4. FILTER AND RANKING QUERIES %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

consult_qualified :-
    write('Job ID: '), read(JobID),
    filter:qualified_candidates(JobID, List),
    ( List == [] ->
        write('No qualified candidates found.'), nl
    ;
        write('Qualified candidates:'), nl,
        forall(member(ID, List),
               (candidate(ID, Name, _, _, _, _, _, _),
                write('ID: '), write(ID), write(' - '), writeln(Name)))
    ).

consult_best :-
    write('Job ID: '), read(JobID),
    write('How many candidates to list? '), read(N),
    score:best_candidates(JobID, N, Best),
    ( Best == [] ->
        write('No qualified candidates found.'), nl
    ;
        write('Best candidates:'), nl,
        forall(member(Score-ID, Best),
               (candidate(ID, Name, _, _, _, _, _, _),
                write('ID: '), write(ID), write(' - '), write(Name),
                write(' (Score: '), write(Score), writeln(')')))
    ).