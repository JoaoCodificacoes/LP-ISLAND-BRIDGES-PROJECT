% +--------------------------+
% Joao David Natario Ferreira|
% ist1103680                 |
% +--------------------------+

:- [codigo_comum].

% +------------------------------------------------------------------------------------+
% extrai_ilhas_linha(N_L, Linha, Ilhas)                                                |
% N_L - inteiro correspondente ao numero da linha                                      |
% Linha - lista correspondente a uma linha de um Puzzle                                |
% Ilhas - lista ordenada cujos elementos sao as ilhas de uma linha do Puzzle           |
% +------------------------------------------------------------------------------------+

%entrar no predicado auxiliar extrai_ilhas_linha/5

extrai_ilhas_linha(N_L,Linha,Ilhas) :- extrai_ilhas_linha(N_L,Linha,Ilhas,1,[]).

extrai_ilhas_linha(_,[],Ilhas,_,Ilhas).

extrai_ilhas_linha(N_L,[P|R],Ilhas_F,Aux,Ilha):-
    P>0,!,
    Aux1 is Aux +1,
    append(Ilha,[ilha(P, (N_L,Aux))],NewIlha),
    extrai_ilhas_linha(N_L,R,Ilhas_F,Aux1,NewIlha).
extrai_ilhas_linha(N_L,[_|R],Ilhas_F,Aux,Ilha):- 
    Aux1 is Aux + 1,
    extrai_ilhas_linha(N_L,R,Ilhas_F,Aux1,Ilha).

% +------------------------------------------------------------------------------------+
% ilhas(Puz,Ilhas)                                                                     |
% Puz - Puzzle                                                                         |
% Ilhas - lista ordenada cujos elementos sao as ilhas de um Puzzle                     |
% +------------------------------------------------------------------------------------+

%entrar no predicado auxiliar ilhas/4

ilhas(Puz,Ilhas):- ilhas(Puz,1,[],Ilhas).

ilhas([],_,Aux,Aux).

ilhas([P|R],N,Aux,Ilhas):-
    extrai_ilhas_linha(N,P,IlhasL),
    append(Aux,IlhasL,Aux1),
    Num is N+1,
    ilhas(R,Num,Aux1,Ilhas).


%predicado que devolve a posicao a partir da ilha

ilhaPos(Ilha,Pos):-
    Ilha = ilha(_, Pos).

%predicado que devolve a linha a partir da ilha

ilhaL(Ilha,Linha):-
    Ilha = ilha(_, (Linha,_)).

%predicado que devolve a coluna a partir da ilha

ilhaC(Ilha,Col):-
    Ilha = ilha(_, (_,Col)).

%predicado que verifica se eh lista vazia

vazia([]).

% +------------------------------------------------------------------------------------+
% |vizinhas(Ilhas, Ilha, Vizinhas)                                                     |
% |Ilhas - lista de ilhas do Puzzle                                                    |
% |Ilha - ilha pertencente a Ilhas                                                     |
% |Vizinhas - lista ordenada cujos elementos sao a ilhas vizinhas de Ilha              |
% +------------------------------------------------------------------------------------+


%predicado que verifica se a ilha se apresenta na mesma linha

vizinhasAuxL(Ilha1,Ilha2):-
        ilhaL(Ilha1,L1),
        ilhaL(Ilha2,L2),
        L1 =:= L2.
%predicado que verifica se a ilha se apresenta na mesma coluna

vizinhasAuxC(Ilha1,Ilha2):-
        ilhaC(Ilha1,C1),
        ilhaC(Ilha2,C2),
        C1 =:= C2.

vizinhas(Ilhas,Ilha,Vizinhas):-
        include(vizinhasAuxL(Ilha),Ilhas,Lst),
        Lst = [P|_],
        last(Lst,L),
        (Ilha == P -> 
            LViz1 = []
            ;
            nextto(LViz1,Ilha,Lst)),
        (Ilha == L ->
            LViz2 = []
            ;
            nextto(Ilha,LViz2,Lst)),
        include(vizinhasAuxC(Ilha),Ilhas,Lst2),
        Lst2 = [H|_],
        last(Lst2,U),
        (Ilha == H -> 
            CViz1 = []
            ;
            nextto(CViz1,Ilha,Lst2)),
        (Ilha == U ->
            CViz2 = []
            ;
            nextto(Ilha,CViz2,Lst2)),
        Aux = [CViz1,LViz1,LViz2,CViz2],
        exclude(vazia,Aux,Vizinhas).

        
% +------------------------------------------------------------------------------------+
% |estado(Ilhas, Estado)                                                               |
% |Ilhas - lista de ilhas de um certo Puzzle                                           |
% |Estado - lista ordenada de entradas de cada uma das ilhas de um Puzzle              |
% +------------------------------------------------------------------------------------+

%entrar no predicado auxiliar estado/4

estado(Ilhas,Estado):- estado(Ilhas,Ilhas,[],Estado).
estado([],_,Estado,Estado).
estado([P|R],Ilhas,Aux,Estado):- 
    vizinhas(Ilhas,P,X),
    append(Aux,[[P,X,[]]],Aux2),
    estado(R,Ilhas,Aux2,Estado).

% +------------------------------------------------------------------------------------+
% |posicoes_entre(Pos1, Pos2, Posicoes)                                                |
% |Pos1 - posicao de uma ilha no Puzzle                                                |
% |Pos2 - posicao de uma ilha no Puzzle                                                |
% |Posicoes - lista ordenada de posicoes entre a Pos1 e Pos2                           |
% +------------------------------------------------------------------------------------+

%caso as posicoes estejam na mesma linha

posicoes_entre((L,C1),(L,C2),Posicoes) :- 
     max_list([C1,C2],P1),
     min_list([C1,C2],P2),
     Max is P1-1,
     Min is P2+1,
     findall((L,Y),between(Min,Max,Y),Posicoes).

%caso as posicoes estejam na mesma coluna

posicoes_entre((L1,C),(L2,C),Posicoes) :- 
     max_list([L1,L2],P1),
     min_list([L1,L2],P2),
     Max2 is P1-1,
     Min2 is P2+1,
     findall((X,C),between(Min2,Max2,X),Posicoes).


% +------------------------------------------------------------------------------------+
% |cria_ponte(Pos1, Pos2, Ponte)                                                       |
% |Pos1 - posicao de uma ilha no Puzzle                                                |
% |Pos2 - posicao de uma ilha no Puzzle                                                |
% |Ponte - ponte entre a Pos1 e Pos2                                                   |
% +------------------------------------------------------------------------------------+

cria_ponte((L1,C1),(L2,C2),Ponte) :-
    (L1<L2 ; C1<C2) ->  (Ponte = ponte((L1,C1), (L2,C2)));
    (Ponte = ponte((L2,C2), (L1,C1))).

% +------------------------------------------------------------------------------------+
% |caminho_livre(Pos1, Pos2, Posicoes, I, Vz)                                          |
% |Pos1 - posicao de uma ilha no Puzzle                                                |
% |Pos2 - posicao de uma ilha no Puzzle                                                |
% |Posicoes - lsita ordenada de posicoes entre Pos1 e Pos2                             |
% |I - ilha de um Puzzle                                                               |
% |Vz - ilha vizinha de I                                                              |
% |Devolve: true (se criar uma Ponte entre Pos1 e Pos2 nao fizer com que I e Vz deixem |
% |de ser vizinhas) ou false (caso contrario)                                          |
% +------------------------------------------------------------------------------------+

caminho_livre(Pos1,Pos2,_,I,Vz):-
    ilhaPos(I,PosI),ilhaPos(Vz,PosVz),
    Pos1 == PosI, Pos2 == PosVz,!.

caminho_livre(Pos1,Pos2,_,I,Vz):-
    ilhaPos(I,PosI),ilhaPos(Vz,PosVz),
    Pos1 == PosVz, Pos2 == PosI,!.

caminho_livre(_,_,Posicoes,I,Vz):-
    ilhaPos(I,PosI),ilhaPos(Vz,PosVz),
    posicoes_entre(PosI,PosVz,Posicoes2),
    subtract(Posicoes,Posicoes2,Aux),
    Posicoes == Aux.


% +------------------------------------------------------------------------------------+
% |actualiza_vizinhas_entrada(Pos1, Pos2, Posicoes, Entrada, Nova_Entrada)             |
% |Pos1 - posicao de uma ilha no  Puzzle                                               |
% |Pos2 - posicao de uma ilha no  Puzzle                                               |
% |Posicoes - lista ordenada de posicoes entre Pos1 e Pos2                             |
% |Entrada - entrada associada ao estado do Puzzle                                     |
% |Nova_Entrada - entrada resultante de remover da lista de ilhas vizinhas de Entrada  |
% |as ilhas que deixaram de ser vizinhas apos a adicao da ponte entre Pos1 e Pos2      |
% +------------------------------------------------------------------------------------+

%entrar no predicado auxiliar actualiza_vizinhas_entrada/6

actualiza_vizinhas_entrada(Pos1,Pos2,Posicoes,[I,Vizs,P],Nova_Entrada):-
actualiza_vizinhas_entrada(Pos1,Pos2,Posicoes,[I,Vizs,P],Vizs,Nova_Entrada).

actualiza_vizinhas_entrada(_,_,_,[I,[],P],Aux,[I,Aux,P]).

actualiza_vizinhas_entrada(Pos1,Pos2,Posicoes,[I,[P|R],Ponte],Aux,Nova_Entrada):-
    \+caminho_livre(Pos1,Pos2,Posicoes,I,P),
    subtract(Aux,[P],Aux1),
    actualiza_vizinhas_entrada(Pos1,Pos2,Posicoes,[I,R,Ponte],Aux1,Nova_Entrada).

actualiza_vizinhas_entrada(Pos1,Pos2,Posicoes,[I,[_|R],Ponte],Aux,Nova_Entrada):-
    actualiza_vizinhas_entrada(Pos1,Pos2,Posicoes,[I,R,Ponte],Aux,Nova_Entrada).


% +------------------------------------------------------------------------------------+
% |actualiza_vizinhas_apos_pontes(Pos_1, Pos_2, Entrada, Nova_Entrada)                 |
% |Pos1 - posicao de uma ilha no Puzzle                                                |
% |Pos2 - posicao de uma ilha no Puzzle                                                |
% |Estado - estado do Puzzle                                                           |
% |Novo_Estado - estado resultante de aplicar o actualiza_vizinhas_entrada a cada uma  |
% |das entradas de Estado                                                              |
% +------------------------------------------------------------------------------------+

actualiza_vizinhas_apos_pontes(Estado, Pos1, Pos2, Novo_Estado):-
        posicoes_entre(Pos1,Pos2,Posicoes),
        maplist(actualiza_vizinhas_entrada(Pos1,Pos2,Posicoes),Estado,Novo_Estado).


% +------------------------------------------------------------------------------------+
% |ilhas_terminadas(Estado, Ilhas_Term)                                                |
% |Estado - estado de um certo Puzzle                                                  |
% |Ilhas_Term - lista de ilhas com todas as pontes associadas                          |
% +------------------------------------------------------------------------------------+

%entrar no predicado auxiliar ilhas_terminadas/4

ilhas_terminadas(Estado,Ilhas_term) :-  ilhas_terminadas(Estado,[],Ilhas_term).

ilhas_terminadas([],Aux,Aux).

ilhas_terminadas([[Ilha,_,Pontes]|R],Aux,Ilhas_term):-
    Ilha = ilha(N_Pontes, _),
    N_Pontes \== 'X',
    length(Pontes,N_Pontes),
    append(Aux,[Ilha],Aux1),
    ilhas_terminadas(R,Aux1,Ilhas_term).

ilhas_terminadas([_|R],Aux,Ilhas_term):-
    ilhas_terminadas(R,Aux,Ilhas_term).


% +------------------------------------------------------------------------------------+
% |tira_ilhas_terminadas_entrada(Ilhas_Term, Entrada, Nova_Entrada)                    |
% |Ilhas_Term - lista de ilhas com todas as pontes associadas                          |
% |Entrada - entrada associada ao estado do Puzzle                                     |
% |Nova_Entrada - entrada resultante de remover as ilhas em Ilhas_Term da lista de     |
% |ilhas vizinhas de Entrada                                                           |
% +------------------------------------------------------------------------------------+

tira_ilhas_terminadas_entrada(Ilhas_term,[Ilha,Vizs,Pontes],[Ilha,Vizs2,Pontes]):-
    subtract(Vizs,Ilhas_term,Vizs2).

% +------------------------------------------------------------------------------------+
% |tira_ilhas_terminadas(Estado, Ilhas_Term, Novo_Estado)                              |
% |Estado - estado do Puzzle                                                           |
% |Ilhas_Term - lista de ilhas com todas as pontes associadas                          |
% |Novo_Estado - estado resultante de aplicar o predicado tira_ilhas_terminadas_entrada|
% |a cada uma das entradas de Estado                                                   |
% +------------------------------------------------------------------------------------+

tira_ilhas_terminadas(Estado,Ilhas_term,Novo_Estado):-
    maplist(tira_ilhas_terminadas_entrada(Ilhas_term),Estado,Novo_Estado).



% +------------------------------------------------------------------------------------+
% |marca_ilhas_terminadas_entrada(Ilhas_Term, Entrada, Nova_Entrada)                   |
% |Ilhas_Term - lista de ilhas com todas as pontes associadas                          |
% |Entrada - entrada associada ao estado do Puzzle                                     |
% |Nova_Entrada - entrada resultante da substituicao do numero de pontes da ilha       |
% |de Entrada por 'X' caso essa ilha pertenca a lista Ilhas_Term ou manter a Entrada   |
% |em caso contrario                                                                   |
% +------------------------------------------------------------------------------------+

marca_ilhas_terminadas_entrada(Ilhas_term,[Ilha,Viz,Pontes],[NewIlha,Viz,Pontes]):-
    member(Ilha,Ilhas_term),
    Ilha = ilha(_, Pos),
    NewIlha = ilha('X', Pos).

marca_ilhas_terminadas_entrada(_,Entrada,Entrada).


% +------------------------------------------------------------------------------------+
% |marca_ilhas_terminadas(Estado, Ilhas_Term, Novo_Estado)                             |
% |Estado - estado do Puzzle                                                           |
% |Ilhas_Term - lista de ilhas com todas as pontes associadas                          |
% |Novo_Estado - estado resultante de aplicar o predicado                              |
% |marca_ilhas_terminadas_entrada a cada uma das entradas de Estado                    |
% +------------------------------------------------------------------------------------+

marca_ilhas_terminadas(Estado,Ilhas_term,Novo_Estado):-
    maplist(marca_ilhas_terminadas_entrada(Ilhas_term),Estado,Novo_Estado).


% +------------------------------------------------------------------------------------+
% |trata_ilhas_terminadas(Estado, Novo_Estado)                                         |
% |Estado - estado do Puzzle                                                           |
% |Novo_Estado - estado resultante de aplicar os predicados tira_ilhas_terminadas e    |
% |marca_ilhas_terminadas a Estado                                                     |
% +------------------------------------------------------------------------------------+
trata_ilhas_terminadas(Estado, Novo_Estado):-
    ilhas_terminadas(Estado,Ilhas_Term),
    marca_ilhas_terminadas(Estado, Ilhas_Term,Ilhas_Marc),
    tira_ilhas_terminadas(Ilhas_Marc, Ilhas_Term, Novo_Estado).

% +------------------------------------------------------------------------------------+
% |junta_pontes(Estado, Num_Pontes, Ilha1, Ilha2, Estado_Final)                        |
% |Estado - estado do Puzzle                                                           |
% |Num_Pontes - inteiro correspondente ao numero de pontes a adicionar                 |
% |Ilha1 - ilha do Puzzle                                                              |
% |Ilha2 - ilha do Puzzle                                                              |
% |Estado_Final - estado resultante da adicao de Num_Pontes pontes entre a Ilha1 e     |
% |Ilha_2 ao Estado                                                                    |
% +------------------------------------------------------------------------------------+

% predicado que devolve a ilha a partir da entrada

retira_ilhas_entrada([Ilha,_,_],Ilha).

% predicado que devolve a lista de Ilhas de um Estado resultante de aplicar 
% retira_ilhas_entrada a todas as entradas do Estado

retira_ilhas(Estado,Ilhas):-
    maplist(retira_ilhas_entrada,Estado,Ilhas).


% predicado que cria uma lista com N numero de Pontes

cria_pontes(Ilha1,Ilha2,N,Pontes):-
    ilhaPos(Ilha1,Pos1),ilhaPos(Ilha2,Pos2),
    cria_ponte(Pos1,Pos2,Ponte),
    length(Pontes,N),
    maplist(=(Ponte),Pontes).


% predicado que devolve um Estado com as pontes de Ilha atualizadas e a sua posicao
% a partir de uma lista de Ilhas, a Ilha, o Estado, e uma lista de pontes a adicionar

junta_pontes_aux(Ilhas,Ilha,Estado,Pontes,Estado_Aux,Pos):-
    nth0(I,Ilhas,Ilha),nth0(I,Estado,Entrada),
    Entrada = [_,Viz,Pontes1],
    append(Pontes1,Pontes,P1),
    select(Entrada,Estado,[Ilha,Viz,P1],Estado_Aux),
    ilhaPos(Ilha,Pos).




junta_pontes(Estado,Num_Pontes,Ilha1,Ilha2,Estado_Final):-
    cria_pontes(Ilha1,Ilha2,Num_Pontes,Pontes),
    retira_ilhas(Estado,Ilhas),
    junta_pontes_aux(Ilhas,Ilha1,Estado,Pontes,Estado_Aux,Pos1),
    junta_pontes_aux(Ilhas,Ilha2,Estado_Aux,Pontes,NewEstado,Pos2),
    actualiza_vizinhas_apos_pontes(NewEstado,Pos1,Pos2,Novo_Estado),
    trata_ilhas_terminadas(Novo_Estado,Estado_Final).








