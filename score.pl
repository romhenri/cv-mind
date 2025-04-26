%%% PONTUAÇÃO DE CANDIDATOS %%%

:- module(score, [pontuacao/3, melhores_candidatos/3]).

pontuacao(CandidatoID, VagaID, Pontuacao) :-
    candidato(CandidatoID, _, _, _, _, ProExpYears, Techs, _),
    vaga(VagaID, _, _, _, _, _, Obrigatorias, Desejaveis, ExpMin),

    % Pontuação por experiência além do mínimo
    PontosExp is max(0, ProExpYears - ExpMin),

    % Pontuação pelas habilidades obrigatórias dominadas
    intersection(Obrigatorias, Techs, TecnologiasObrigatorias),
    length(TecnologiasObrigatorias, PontosObrigatorias),

    % Pontuação pelas habilidades desejáveis dominadas
    intersection(Techs, Desejaveis, TecnologiasDesejaveis),
    length(TecnologiasDesejaveis, PontosDesejaveis),

    % Pontuação total
    Pontuacao is PontosExp + PontosObrigatorias + PontosDesejaveis.

melhores_candidatos(VagaID, N, Melhores) :-
    findall(Pont-Cand, (
        filter:tem_qualificacao_minima(Cand, VagaID),
        pontuacao(Cand, VagaID, Pont)
    ), Candidatos),
    sort(1, @>=, Candidatos, Ordenados),
    pegar_primeiros(N, Ordenados, Melhores).

pegar_primeiros(N, Lista, Primeiros) :-
    length(Primeiros, N),
    append(Primeiros, _, Lista).
