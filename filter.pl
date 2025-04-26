%%% FILTRO DE CANDIDATOS %%%

:- module(filter, [tem_qualificacao_minima/2, candidatos_qualificados/2]).

tem_qualificacao_minima(CandidatoID, VagaID) :-
    candidato(CandidatoID, _, CRA, Periodo, Curso, _, _, Interests),
    vaga(VagaID, _, AreaInteresse, CursoAlvo, CRA_Min, PeriodoMin, _, _, _),

    % Filtros obrigatórios
    CRA >= CRA_Min,
    Periodo >= PeriodoMin,
    Curso == CursoAlvo,
    member(AreaInteresse, Interests).

candidatos_qualificados(VagaID, Lista) :-
    findall(CandidatoID, tem_qualificacao_minima(CandidatoID, VagaID), Lista).
