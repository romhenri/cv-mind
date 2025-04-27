:- dynamic job/9.

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
    write('5. Input a new job'), nl,
    write('0. Exit'), nl,
    write('Option: '),
    read(Option),
    handle_option(Option).

handle_option(0) :- !, write('Exiting system...'), nl.
handle_option(1) :- list_jobs, menu.
handle_option(2) :- list_candidates, menu.
handle_option(3) :- consult_qualified, menu.
handle_option(4) :- consult_best, menu.
handle_option(5) :- input_job, menu.
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
%%% 4. JOB INPUT FUNCTIONALITY %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

input_job :-
    nl,
    write('=== INPUT NEW JOB DETAILS ==='), nl,
    flush_output,
    
    % Get new job ID
    write('Enter job ID: '),
    flush_output,
    read(ID),
    skip_line,
    
    % Verify if ID already exists
    (job(ID, _, _, _, _, _, _, _, _) ->
        (write('Error: Job ID already exists!'), nl, nl, menu)
    ;
        % Collect all job details
        write('Job title: '), 
        read_line_to_string(user_input, Title),
        
        write('Interest area: '),
        read_line_to_string(user_input, Area),
        
        write('Target course (ex: "Computer Engineering"): '),
        read_line_to_string(user_input, Course),
        
        write('Minimum CRA: '),
        read_number(MinCRA),
        
        write('Minimum semester: '),
        read_number(MinSemester),
        
        write('Required skills (comma separated, e.g., "python,sql,docker"): '),
        read_line_to_string(user_input, RequiredStr),
        split_string(RequiredStr, ",", " ", RequiredParts),
        maplist(string_trim, RequiredParts, RequiredList),
        
        write('Desired skills (comma separated, e.g., "aws,git,ml"): '),
        read_line_to_string(user_input, DesiredStr),
        split_string(DesiredStr, ",", " ", DesiredParts),
        maplist(string_trim, DesiredParts, DesiredList),
        
        write('Minimum experience (years): '),
        read_number(MinExp),
        
        % Assert the new job
        assertz(job(ID, Title, Area, Course, MinCRA, MinSemester, RequiredList, DesiredList, MinExp)),
        
        % Display confirmation
        nl, write('New job successfully added!'), nl, nl,
        display_job_details(ID, Title, Area, Course, MinCRA, MinSemester, RequiredList, DesiredList, MinExp),
        
        % Mostrar candidatos qualificados
        nl, write('=== QUALIFIED CANDIDATES FOR THIS JOB ==='), nl,
        (filter:qualified_candidates(ID, QualifiedList) ->
            (QualifiedList == [] ->
                write('No qualified candidates found.'), nl
            ;
                forall(member(CandID, QualifiedList),
                    (candidate(CandID, Name, _, _, _, _, _, _),
                     write('ID: '), write(CandID), write(' - '), writeln(Name)))
            ),
            
            % Chamar diretamente a lógica de melhores candidatos (igual à opção 4)
            nl, nl, write('=== BEST CANDIDATES FOR THIS JOB ==='), nl,
            write('How many candidates to list? '), read(N), nl,
            score:best_candidates(ID, N, Best),
            ( Best == [] ->
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

% Helper predicates
read_number(Number) :-
    read_line_to_string(user_input, Input),
    (number_string(Number, Input) -> true ; (write('Invalid number! Try again: '), read_number(Number))).

string_trim(String, Trimmed) :-
    string_chars(String, Chars),
    exclude(=(' '), Chars, TrimmedChars),
    string_chars(Trimmed, TrimmedChars).

display_job_details(ID, Title, Area, Course, MinCRA, MinSemester, Required, Desired, MinExp) :-
    write('=== JOB DETAILS ==='), nl,
    write('ID: '), write(ID), nl,
    write('Title: '), write(Title), nl,
    write('Area: '), write(Area), nl,
    write('Target Course: '), write(Course), nl,
    write('Minimum CRA: '), write(MinCRA), nl,
    write('Minimum Semester: '), write(MinSemester), nl,
    write('Required Skills: '), write(Required), nl,
    write('Desired Skills: '), write(Desired), nl,
    write('Minimum Experience: '), write(MinExp), write(' years'), nl, nl.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% 5. FILTER AND RANKING QUERIES %%%
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