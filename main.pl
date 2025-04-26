%%% SISTEMA DE SELEÇÃO DE CURRÍCULOS EM PROLOG %%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% 1. BASE DE DADOS - VAGAS E CANDIDATOS %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- consult('candidates.pl').
:- consult('vagas.pl').
:- use_module(filter).
:- use_module(score).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% 2. INICIALIZAÇÃO DO SISTEMA %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- initialization(iniciar_sistema).

iniciar_sistema :-
    write('Sistema de Selecao de Curriculos carregado.'), nl,
    write('Digite "menu." para iniciar a interface interativa.'), nl.

menu :-
    nl,
    write('=== SISTEMA DE SELECAO DE CURRICULOS ==='), nl,
    write('1. Listar vagas disponiveis'), nl,
    write('2. Listar candidatos cadastrados'), nl,
    write('3. Ver candidatos qualificados para uma vaga'), nl,
    write('4. Ver melhores candidatos para uma vaga'), nl,
    write('0. Sair'), nl,
    write('Opcao: '),
    read(Opcao),
    tratar_opcao(Opcao).

tratar_opcao(0) :- !, write('Saindo do sistema...'), nl.
tratar_opcao(1) :- listar_vagas, menu.
tratar_opcao(2) :- listar_candidatos, menu.
tratar_opcao(3) :- consultar_qualificados, menu.
tratar_opcao(4) :- consultar_melhores, menu.
tratar_opcao(_) :- write('Opcao invalida!'), nl, menu.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% 3. FUNÇÕES DE LISTAGEM %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

listar_vagas :-
    write('=== VAGAS DISPONIVEIS ==='), nl,
    forall(vaga(ID, Titulo, Area, CursoAlvo, CRA_Min, PeriodoMin, Obrigatorias, Desejaveis, ExpMin),
           (write('ID: '), write(ID), write(' - '), write(Titulo), nl,
            write('  Area: '), write(Area), nl,
            write('  Curso Alvo: '), write(CursoAlvo), nl,
            write('  CRA Minimo: '), write(CRA_Min), nl,
            write('  Periodo Minimo: '), write(PeriodoMin), nl,
            write('  Skills Obrigatorias: '), write(Obrigatorias), nl,
            write('  Skills Desejaveis: '), write(Desejaveis), nl,
            write('  Experiencia Minima: '), write(ExpMin), write(' anos'), nl, nl)).

listar_candidatos :-
    write('=== CANDIDATOS CADASTRADOS ==='), nl,
    forall(candidato(ID, Nome, CRA, Periodo, Curso, ProExpYears, Techs, Interests),
           (write('ID: '), write(ID), write(' - '), write(Nome), nl,
            write('  CRA: '), write(CRA), nl,
            write('  Periodo: '), write(Periodo), nl,
            write('  Curso: '), write(Curso), nl,
            write('  Experiencia Profissional: '), write(ProExpYears), write(' anos'), nl,
            write('  Tecnologias: '), write(Techs), nl,
            write('  Areas de Interesse: '), write(Interests), nl, nl)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% 4. CONSULTAS DE FILTRO E RANKING %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

consultar_qualificados :-
    write('ID da Vaga: '), read(VagaID),
    filter:candidatos_qualificados(VagaID, Lista),
    ( Lista == [] ->
        write('Nenhum candidato qualificado encontrado.'), nl
    ;
        write('Candidatos qualificados:'), nl,
        forall(member(ID, Lista),
               (candidato(ID, Nome, _, _, _, _, _, _),
                write('ID: '), write(ID), write(' - '), writeln(Nome)))
    ).

consultar_melhores :-
    write('ID da Vaga: '), read(VagaID),
    write('Quantos candidatos deseja listar? '), read(N),
    score:melhores_candidatos(VagaID, N, Melhores),
    ( Melhores == [] ->
        write('Nenhum candidato qualificado encontrado.'), nl
    ;
        write('Melhores candidatos:'), nl,
        forall(member(Pont-ID, Melhores),
               (candidato(ID, Nome, _, _, _, _, _, _),
                write('ID: '), write(ID), write(' - '), write(Nome),
                write(' (Pontuacao: '), write(Pont), writeln(')')))
    ).
