%%% CANDIDATE FILTER %%%

:- module(filter, [has_minimum_qualifications/2, qualified_candidates/2]).

has_minimum_qualifications(CandidateID, JobID) :-
    candidate(CandidateID, _, CRA, Semester, Course, _, _, Interests),
    job(JobID, _, InterestAreaNeeded, TargetCourse, Min_CRA_Needed, Min_SemesterNeeded, _, _, _),

    % Mandatory filters
    CRA >= Min_CRA_Needed,
    Semester >= Min_SemesterNeeded,
    Course == TargetCourse,
    member(InterestAreaNeeded, Interests).

qualified_candidates(JobID, List) :-
    findall(CandidateID, has_minimum_qualifications(CandidateID, JobID), List).