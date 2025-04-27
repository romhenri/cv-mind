%%% CANDIDATE FILTER %%%

:- module(filter, [has_minimum_qualifications/2, qualified_candidates/2]).

has_minimum_qualifications(CandidateID, JobID) :-
    % Get candidate data
    candidates:candidate(CandidateID, _, CRA, Semester, CandidateCourse, _, _, CandidateInterests),
    
    % Get job data
    job(JobID, _, JobInterestArea, JobTargetCourse, MinCRA, MinSemester, _, _),
    
    % 1. Check academic requirements
    CRA >= MinCRA,
    Semester >= MinSemester,
    
    % 2. Check course match (case and space insensitive)
    normalize_string(CandidateCourse, NormalizedCandidateCourse),
    normalize_string(JobTargetCourse, NormalizedJobCourse),
    NormalizedCandidateCourse == NormalizedJobCourse,
    
    % 3. Check interest area match (case insensitive)
    normalize_interest_areas(CandidateInterests, NormalizedCandidateInterests),
    normalize_string(JobInterestArea, NormalizedJobInterest),
    member(NormalizedJobInterest, NormalizedCandidateInterests).
    
% String normalization (lowercase and remove spaces)
normalize_string(String, Normalized) :-
    atom_string(String, Str),
    string_lower(Str, Lower),
    split_string(Lower, " ", "", Parts),
    atomic_list_concat(Parts, "", Normalized).

% Technology normalization
normalize_tech(Tech, Normalized) :-
    normalize_string(Tech, Normalized).

% Interest areas normalization
normalize_interest_areas(Areas, Normalized) :-
    maplist(normalize_string, Areas, Normalized).

% Find all qualified candidates
qualified_candidates(JobID, QualifiedList) :-
    findall(CandidateID, 
            has_minimum_qualifications(CandidateID, JobID), 
            QualifiedList).