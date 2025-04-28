:- module(util, [generate_job_id/1]).

generate_job_id(ID) :-
    findall(ExistingID, job(ExistingID, _, _, _, _, _, _, _), IDs),
    ( IDs == [] ->
        ID = 1
    ;
        max_list(IDs, MaxID),
        ID is MaxID + 1
    ).
