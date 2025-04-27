%%% RESUME SELECTION SYSTEM IN PROLOG %%%

:- dynamic job/8.
:- dynamic candidate/8.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% 1. MODULES AND DATABASES %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- consult('data/candidates.pl').
:- consult('data/jobs.pl').

:- use_module('services/filter').
:- use_module('services/score').

:- use_module('util/io').
:- use_module('util/util').

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
    write('1. Input a job for recommendation'), nl,
    write('2. List available jobs'), nl,
    write('3. List registered candidates'), nl,
    write('4. Filter qualified candidates for a job'), nl,
    write('5. Recommend best candidates for a job'), nl,
    write('0. Exit'), nl,
    io:ask_option('Option: ', Option),
    handle_option(Option).

handle_option(1) :- input_job, menu.
handle_option(2) :- list_jobs, menu.
handle_option(3) :- list_candidates, menu.
handle_option(4) :- consult_qualified, menu.
handle_option(5) :- consult_best, menu.
handle_option(0) :- !, write('Exiting system...'), nl.
handle_option(_) :- write('Invalid option!'), nl, menu.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% 3. LISTING FUNCTIONS %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

list_jobs :-
    write('=== AVAILABLE JOBS ==='), nl,
    forall(job(ID, Title, Area, TargetCourse, Min_CRA, Min_Semester, Desired, Min_Exp),
           (io:print_job(ID, Title, Area, TargetCourse, Min_CRA, Min_Semester, Desired, Min_Exp),
            io:print_list_separator)).

list_candidates :-
    write('=== REGISTERED CANDIDATES ==='), nl,
    forall(candidate(ID, Name, CRA, Semester, Course, ProExpYears, Techs, Interests),
           (io:print_candidate(ID, Name, CRA, Semester, Course, ProExpYears, Techs, Interests),
            io:print_list_separator)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% 4. JOB INPUT FUNCTIONALITY %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

input_job :-
    nl,
    write('=== INPUT NEW JOB DETAILS ==='), nl,

    generate_job_id(ID),
    format('Generated Job ID: ~w~n', [ID]),

    % Collect all job details
    io:ask_string('Job title: ', Title),
    io:ask_string('Interest area: ', Area),
    io:ask_string('Target course: ', Course),
    io:ask_number('Minimum CRA: ', MinCRA),
    io:ask_number('Minimum semester: ', MinSemester),
    io:ask_list_of_strings('Desired skills (comma separated): ', DesiredList),
    io:ask_number('Minimum experience (years): ', MinExp),

    % Validation step
    ( invalid_input(Title, Area, Course, MinCRA, MinSemester, MinExp) ->
        write('Input canceled due to invalid fields.'), nl, menu
    ;
        % Otherwise, create the job
        assertz(job(ID, Title, Area, Course, MinCRA, MinSemester, DesiredList, MinExp)),

        % Confirmation
        nl, write('New job successfully added!'), nl,
        io:print_list_separator,
        io:print_job(ID, Title, Area, Course, MinCRA, MinSemester, DesiredList, MinExp),
        io:print_list_separator,

        % Show qualified candidates
        nl, write('=== QUALIFIED CANDIDATES FOR THIS JOB ==='), nl,
        (filter:qualified_candidates(ID, QualifiedList) ->
            (QualifiedList == [] ->
                write('No qualified candidates found.'), nl
            ;
                forall(member(CandID, QualifiedList),
                       (candidate(CandID, Name, _, _, _, _, _, _),
                        write('ID: '), write(CandID), write(' - '), writeln(Name)))
            ),

            % Show best candidates
            nl, nl, write('=== BEST CANDIDATES FOR THIS JOB ==='), nl,
            io:ask_number('How many candidates to list? ', N),
            score:best_candidates(ID, N, Best),
            (Best == [] ->
                write('No qualified candidates found.'), nl
            ;
                forall(member(Score-CandID, Best),
                       (candidate(CandID, Name, _, _, _, _, _, _),
                        write('ID: '), write(CandID), write(' - '), write(Name),
                        write(' (Score: '), write(Score), writeln(')')))
            )
        ;
            write('Error checking qualified candidates.'), nl
        ),
        nl
    ).

% === Helper to validate input ===
invalid_input(Title, Area, Course, MinCRA, MinSemester, MinExp) :-
    ( Title == ""
    ; Area == ""
    ; Course == ""
    ; MinCRA < 0
    ; MinSemester < 1
    ; MinExp < 0
    ).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% 5. FILTER AND RANKING QUERIES %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

consult_qualified :-
    io:ask_number('Job ID: ', JobID),
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
    io:ask_number('Job ID: ', JobID),
    io:ask_number('How many candidates to list? ', N),
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
