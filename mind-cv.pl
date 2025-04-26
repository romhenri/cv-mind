%%% SISTEMA DE SELEÇÃO DE CURRÍCULOS EM PROLOG %%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% 1. BASE DE DADOS - VAGAS E CANDIDATOS %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

vaga(1, 'Desenvolvedor Backend', ['Python', 'SQL'], ['Docker', 'AWS', 'Git'], 2).
vaga(2, 'Analista de Dados', ['SQL', 'Excel'], ['Python', 'Power BI', 'Estatistica'], 1).
vaga(3, 'Cientista de Dados', ['Python', 'Estatistica'], ['Machine Learning', 'R', 'SQL'], 3).

candidato(101, 'Joao Silva', ['Python', 'SQL', 'Git', 'AWS'], 3, 'Ciencia da Computacao').
candidato(102, 'Maria Souza', ['Excel', 'SQL', 'Power BI'], 2, 'Administracao').
candidato(103, 'Carlos Oliveira', ['Python', 'AWS', 'Docker', 'Machine Learning'], 4, 'Engenharia').

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% 2. REGRAS DE TRIAGEM E CLASSIFICAÇÃO %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

tem_qualificacao_minima(CandidatoID, VagaID) :-
    candidato(CandidatoID, _, Habilidades, Exp, _),
    vaga(VagaID, _, Obrigatorias, _, ExpMin),
    subset(Obrigatorias, Habilidades),
    Exp >= ExpMin.

pontuacao(CandidatoID, VagaID, Pontuacao) :-
    candidato(CandidatoID, _, Habilidades, Exp, _),
    vaga(VagaID, _, _, Desejaveis, ExpMin),
    PontosExp is max(0, Exp - ExpMin),
    intersection(Habilidades, Desejaveis, HabilidadesComuns),
    length(HabilidadesComuns, PontosHabs),
    Pontuacao is PontosExp + PontosHabs.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% 3. CONSULTAS PARA RECOMENDAÇÃO %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

candidatos_qualificados(VagaID, Lista) :-
    findall(CandidatoID, tem_qualificacao_minima(CandidatoID, VagaID), Lista).

melhores_candidatos(VagaID, N, Melhores) :-
    findall(Pont-Cand, (
        tem_qualificacao_minima(Cand, VagaID),
        pontuacao(Cand, VagaID, Pont)
    ), Candidatos),
    sort(1, @>=, Candidatos, Ordenados),
    pegar_primeiros(N, Ordenados, Melhores).

pegar_primeiros(N, Lista, Primeiros) :-
    length(Primeiros, N),
    append(Primeiros, _, Lista).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% 4. INICIALIZAÇÃO DO SISTEMA %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- write('Sistema de Selecao de Curriculos carregado.'), nl,
   write('Digite "menu." para iniciar a interface interativa.'), nl.

menu :-
    nl,
    write('=== SISTEMA DE SELECAO DE CURRICULOS ==='), nl,
    write('1. Listar vagas disponiveis'), nl,
    write('2. Listar candidatos'), nl,
    write('3. Ver candidatos qualificados para uma vaga'), nl,
    write('4. Ver melhores candidatos para uma vaga'), nl,
    write('0. Sair'), nl,
    write('Opcao: '),
    read(Opcao),
    (Opcao == 0 -> true;
     Opcao == 1 -> listar_vagas, menu;
     Opcao == 2 -> listar_candidatos, menu;
     Opcao == 3 -> consultar_qualificados, menu;
     Opcao == 4 -> consultar_melhores, menu;
     write('Opcao invalida!'), nl, menu).

listar_vagas :-
    write('=== VAGAS DISPONIVEIS ==='), nl,
    forall(vaga(ID, Titulo, _, _, _), 
           (write('ID: '), write(ID), write(' - '), writeln(Titulo))).

listar_candidatos :-
    write('=== CANDIDATOS CADASTRADOS ==='), nl,
    forall(candidato(ID, Nome, _, Exp, Formacao), 
           (write('ID: '), write(ID), write(' - '), write(Nome),
            write(' (Exp: '), write(Exp), write(' anos, '),
            write('Formacao: '), write(Formacao), writeln(')'))).

consultar_qualificados :-
    write('ID da Vaga: '), read(VagaID),
    candidatos_qualificados(VagaID, Lista),
    write('Candidatos qualificados:'), nl,
    forall(member(ID, Lista), 
           (candidato(ID, Nome, _, _, _),
            write('ID: '), write(ID), write(' - '), writeln(Nome))).

consultar_melhores :-
    write('ID da Vaga: '), read(VagaID),
    write('Quantos candidatos deseja listar? '), read(N),
    melhores_candidatos(VagaID, N, Melhores),
    write('Melhores candidatos:'), nl,
    forall(member(Pont-ID, Melhores), 
           (candidato(ID, Nome, _, _, _),
            write('ID: '), write(ID), write(' - '), write(Nome),
            write(' (Pontuacao: '), write(Pont), writeln(')'))).