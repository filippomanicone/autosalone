:- dynamic automobile/1.
:- dynamic accessorio/1.
:- dynamic punteggio/1.
:- dynamic tipo/1.



% prende il punteggio relativo al dato passato
trovaPunteggio([H|_],Componente,Punteggio):-
      is_list(H),
      trovaPunteggio(H,Componente,Punteggio).
trovaPunteggio([_|T],Componente,Punteggio):-
     is_list(T),
     trovaPunteggio(T,Componente,Punteggio).
trovaPunteggio([IdRule|T],IdRule,T).



%toglie gli elementi ripetuti da una lista

unico(L1,L2) :- unico(L1,[],L2).
unico([],ACC,ACC).
unico([H|L1],ACC,L2) :-
   member(H,ACC),
   unico(L1,ACC,L2).
unico([H|L1],ACC,L2) :-
   append(ACC,[H],ACC1),
   unico(L1,ACC1,L2).


%appiattisce lista.
appiatta(L1,L2) :- appiatta(L1,[],L2).
appiatta([],ACC,ACC).
appiatta([H|REST],ACC,L2) :-
    H = [_|_],
    appiatta(H,ACC,ACC1),
    appiatta(REST,ACC1,L2).
    appiatta([H|REST],ACC,L2) :-
    append(ACC,[H],ACC1),
    appiatta(REST,ACC1,L2).
appiatta(X,ACC,L2) :- append(ACC,[X],L2).


%rimpiazza valore lista
rimpiazza(_,_,[],[]).
rimpiazza(1,Valore,[_|VecchiaLista], [Valore|NuovaLista]) :-
     rimpiazza(0, Valore, VecchiaLista, NuovaLista),!.
     rimpiazza(Posizione,Valore,[X|VecchiaLista],[X|NuovaLista]) :-
     Posizione =\= 1,
     NuovaPosizione is Posizione - 1,
     rimpiazza(NuovaPosizione, Valore, VecchiaLista,NuovaLista),!.


%prende i dati dal db e inserisce per ogni lista un sottolista con tutti gli gli accessori dell'auto

getrowsauto(_,[],[]).
getrowsauto(X,[R|Rs], [T|Ts]):-
        enchancerowsauto(X,R,T),
        getrowsauto(X,Rs,Ts).

enchancerowsauto(auto, [N,S,DB,KM,Tipo,_,Prezzo,CO,ID], [ID, N,Data,KM,Tipo,Marca,Prezzo,Colore, T2, Confort1]):-
        getdata(marca,S, row(_, Marca,Confort)),
        trasformadata(DB,Data),
        getdata(colore, CO, row(_,Colore,_)),
        getableacc(ID,RawT),
        getrows(accessoriprof,RawT,T,_),
        appiatta(T,T1),
        unico(T1,T2),
         meccanico(KM, Marca, Confort, Confort1),
        assert(accessorio(T2)),
        assert(tipo(Tipo)).

%trasforma la data in formato GG/MM/AAAA e sottoforma di stringa
trasformadata(date(Y,M,D),Date):-
        appendall([D,M,Y],Date,'.').

%asserta i dati in memoria.
assert_auto([]).
assert_auto([H|T]):-
     assert(automobile(H)),
     write(automobile(H)),
     assert_auto(T).

startengine:-
    abolish(fatti/1),
    abolish(accessorio/1),
    abolish(automobile/1),
    abolish(punteggio/1),
    assert(fatti('lavoro')),
    assert(fatti('budget')),
    assert(fatti('km')),
go.




go:-
   clause(fatti(H),true),
   ask(H),
   retract(fatti(H)).

%visualizza i risultati
go:-
   results.

% aggiorna punteggio accessori
update_confortaccessori([],_).
update_confortaccessori([L|T],A):-
     nth1(9,L,X),
     member(A,X),true,
     getdata(A,row(Confort)),
     atom_number(Confort, CO),
     nth1(10,L,P),
     PF is P + CO,
     rimpiazza(10,PF,L,L1),
     retract(automobile(L)),
     assert(automobile(L1)),
  update_confortaccessori(T,A).



update_confortaccessori([L|T],A):-
    nth1(9,L,X),
    not(member(A,X)),
  update_confortaccessori(T,A).



%aggiorna punteggio in base ai KM
update_punteggioforKm([],_).
update_punteggioforKm([L|T],UKM):-
     nth1(4,L,K),
     nth1(10,L,P),
     NP is UKM/K,
     PF is P + NP,
     rimpiazza(10,PF,L,L1),
     retract(automobile(L)),
     assert(automobile(L1)),
   update_punteggioforKm(T,UKM).

%predicato per individuare a quale categoria lavoratica e membro l'utente
membro(A,Lista):-
  member(A,Lista),true,
  assert(fatti('percorsi')),
  lunghezza(Lista,N),
  N>1,
  assert(fatti('utilizzo')).
membro(_,_).


%predicato per individuare la lunghezza della lista
lunghezza([],0).
lunghezza([_|Coda],N):-
  lunghezza(Coda,Ncoda),
  N is Ncoda + 1.


%asserta lista dei fatti
assertalista([]).
assertalista([H|T]):-
   assert(fatti(H)),
   assertalista(T).
   
   


