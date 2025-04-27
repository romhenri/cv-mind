:- module(util, [generate_job_id/1]).

generate_job_id(NewID) :-
    findall(ID, job(ID, _, _, _, _, _, _, _, _), IDs),
    (IDs == [] ->
        NewID = 1
    ;
        max_list(IDs, MaxID),
        NewID is MaxID + 1
    ).
