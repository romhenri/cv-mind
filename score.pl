%%% CANDIDATE SCORING %%%

:- module(score, [score/3, best_candidates/3]).

score(CandidateID, JobID, Score) :-
    candidate(CandidateID, _, _, _, _, ProExpYears, Techs, _),
    job(JobID, _, _, _, _, _, Required, Desired, Min_Exp),

    % Score for experience beyond minimum
    ExpPoints is max(0, ProExpYears - Min_Exp),

    % Score for required skills
    intersection(Required, Techs, RequiredTechs),
    length(RequiredTechs, RequiredPoints),

    % Score for desired skills
    intersection(Techs, Desired, DesiredTechs),
    length(DesiredTechs, DesiredPoints),

    % Total score
    Score is ExpPoints + RequiredPoints + DesiredPoints.

best_candidates(JobID, N, Best) :-
    findall(Score-Cand, (
        filter:has_minimum_qualifications(Cand, JobID),
        score(Cand, JobID, Score)
    ), Candidates),
    sort(1, @>=, Candidates, Sorted),
    get_first(N, Sorted, Best).

get_first(N, List, First) :-
    length(First, N),
    append(First, _, List).