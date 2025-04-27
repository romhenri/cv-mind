:- module(io, [
    ask_number/2,
    ask_string/2,
    ask_list_of_strings/2,
    ask_option/2,
    print_job/8,
    print_candidate/8,
    print_list_separator/0
]).

% ========== INPUTS ==========

% Ask for a number (using read/1 like your original)
ask_number(Prompt, Number) :-
    write(Prompt),
    flush_output,
    read(Number),
    flush_input_buffer.

% Ask for a string (using read_line_to_string)
ask_string(Prompt, String) :-
    write(Prompt),
    flush_output,
    read_line_to_string(user_input, String).

% Ask for a list of strings (comma separated)
ask_list_of_strings(Prompt, List) :-
    write(Prompt),
    flush_output,
    read_line_to_string(user_input, Input),
    split_string(Input, ",", " ", Parts),
    maplist(string_trim, Parts, List).

% Ask an option (number)
ask_option(Prompt, Option) :-
    ask_number(Prompt, Option).

% After reading a number with read/1, consume leftover newline
flush_input_buffer :-
    set_input(user_input),
    get_char(_).

% ========== OUTPUTS ==========

print_job(ID, Title, Area, Course, MinCRA, MinSemester, DesiredSkills, MinExp) :-
    write('ID: '), write(ID), nl,
    write('Title: '), write(Title), nl,
    write('Area: '), write(Area), nl,
    write('Target Course: '), write(Course), nl,
    write('Minimum CRA: '), write(MinCRA), nl,
    write('Minimum Semester: '), write(MinSemester), nl,
    write('Desired Skills: '), write(DesiredSkills), nl,
    write('Minimum Experience: '), write(MinExp), write(' years'), nl.

print_candidate(ID, Name, CRA, Semester, Course, ProExpYears, Techs, Interests) :-
    write('ID: '), write(ID), write(' - '), write(Name), nl,
    write('  CRA: '), write(CRA), nl,
    write('  Semester: '), write(Semester), nl,
    write('  Course: '), write(Course), nl,
    write('  Professional Experience: '), write(ProExpYears), write(' years'), nl,
    write('  Technologies: '), write(Techs), nl,
    write('  Interests: '), write(Interests), nl.

print_list_separator :-
    write('------------------------------------------'), nl.

% Helper para tirar espaços de strings
string_trim(String, Trimmed) :-
    string_chars(String, Chars),
    exclude(=(' '), Chars, TrimmedChars),
    string_chars(Trimmed, TrimmedChars).
