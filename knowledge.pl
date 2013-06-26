 punteggiolavoro([[operaio,1.1],[impiegato,1.2],[imprenditore,1.4], [studente,1], [disoccupato,1]]).
 ricambicostosi([audi, bmw, mercedes, ferrari, porche, jaguar, lamborghini]).
problemamotore([fiat, 160000]).
problemamotore([volkswagen, 200000]).
problemamotore([ford, 145000]).
problemamotore([tata, 230000]).
problemamotore([lancia, 160000]).


% serve per poter individuare fin quando possiamo incrementare il budget.
ask('lavoro'):-
user('Che lavoro svolgi?', [operaio,impiegato,imprenditore, studente, disoccupato], 'lavoro').

ask('budget'):-
user('Qualè il tuo budget?','budget').

%per verificare se utili o no chiedere determinati accessori
ask('percorsi'):-
user('Che tipologia di percorsi effettui?', ['strade urbane','Strade extraurbane', 'strade extraurbane e percorsi ferrati'], 'percorsi').

%viene chiesto solo se l'utente sceglie strade extraurbane e percorsi sferrati
ask('4wd'):-
user('Desideri una macchian 4WD?', [si,no], '4wd').

%non lo chiediamo a disoccupati o studenti
ask('km'):-
user('Quanti km percorri in un anno?','km').

ask('spaziosa'):-
user('Hai necessita di spazio?', [si,no], 'spaziosa').

ask('famiglia'):-
user('Quanti membri siete nella tua famiglia?', ['<=2', '<=4', '=>5'], 'famiglia').

ask('gancio traino'):-
user('Possiedi caravan o barche?', [si,no], 'gancio traino').


%viene chiesto se non è disoccupato ne studente
ask(utilizzo):-
user('Quale utilizzo fai della macchina?',['prima macchina','seconda macchina','x lavoro'],utilizzo).

ask('figli'):-
user('Hai figli?', [si,no],'figli').

ask(X):-
user(X, [si,no],X).

%  asserta le autombili in memoria in base al budget imposto dall'utente

regola(lavoro,Valore):-
    punteggiolavoro(LP),
    trovaPunteggio(LP,Valore,[Punteggio|_]),
    assert(punteggio(Punteggio)),
    go.


%incremento il budget in base alla tipologia dell'utente e asserto le auto in memoria
regola(budget,Valore):-
   atom_number(Valore, NBudget),
   clause(punteggio(X),true),
   NewBudget is NBudget *X,
   getable(auto,RawT,NewBudget),
   getrowsauto(auto,RawT,T),
   assert_auto(T),
   go.

%verifico la tipolgia di percosi e ne retratto gli accessori da fuoristrada in caso non percorre strade exstraurbane
regola(percorsi,Valore):-
    Valore=='strade extraurbane e percorsi ferrati',
    go.
regola(percorsi,_):-
    retract(fatti('4wd')),
    go.

%valuto le auto in base ai km percorsi e ai km che intende percorrere l'utente
regola(km,KM):-
   atom_number(KM, Km),
   findall(L,automobile(L),ListaAutomobile),
   update_punteggioforKm(ListaAutomobile, Km),
   findall(T,tipo(T),ListaTipo),
   unico(ListaTipo,X),
   membro('fuoristrada',X),
   findall(C,accessorio(C),Listaaccessorio),
   appiatta(Listaaccessorio,AC),
   unico(AC,AC1),
   assertalista(AC1),
   go.

regola(utilizzo,Tipo):-
   Tipo=='x lavoro',
   assert(fatti('spaziosa')),
   go.

regola(utilizzo,Tipo):-
   Tipo=='prima macchina',
   assert(fatti('famiglia')),
   go.
   
%regola spaziosa in base alla necessita di spazio ke desidera l'utente
regola(spaziosa,Tipo):-
   Tipo=='si',
   findall(L,automobile(L),ListaAutomobile),
   write(ListaAutomobile),
   punteggioutilizzo(ListaAutomobile,'famigliare',1.2),
   punteggioutilizzo(ListaAutomobile,'fuoristrada',1.2),
   go.
   
%regola famiglia in base ai componenti selezionati
regola(famiglia,Tipo):-
   Tipo=='=>5',
   findall(L,automobile(L),ListaAutomobile),
   write(ListaAutomobile),
   punteggioutilizzo(ListaAutomobile,'famigliare',1.2),
   go.


regola(famiglia,Tipo):-
   Tipo=='<=4',
   findall(L,automobile(L),ListaAutomobile),
   punteggioutilizzo(ListaAutomobile,'citycar',-1.2),
   go.
   
%si attiva per ogni accessorio e ne aggiorna il punteggio dell'auto in base al loro valore di confort
regola(Y,Valore):-
    Valore=='si',
    findall(L,automobile(L),ListaAutomobile),
    update_confortaccessori(ListaAutomobile, Y),
    go.
 
regola(_,_):-
    go.
    

%predicato per incremento punteggio per le auto consigliate per determinati tipologie di percorsi
punteggioutilizzo([],_,_).
punteggioutilizzo([L|T],Tipo,Punteggio):-
   nth1(5,L,K),
   K==Tipo,
   nth1(10,L,P),
   PF is P + Punteggio,
   rimpiazza(10,PF,L,L1),
   retract(automobile(L)),
   assert(automobile(L1)),
   punteggioutilizzo(T,Tipo,Punteggio).

punteggioutilizzo([L|T],Tipo,Punteggio):-
   punteggioutilizzo(T,Tipo,Punteggio).


% predicato che svolge il ruolo di un esperto indicando i poblemi e i costi di gestioni delle vetture
meccanico(KM, Marca, Confort, Confort2):-
updateKM(KM,Confort,X),
pregimarca(Marca, X, Confort2),
probmotore(Marca, Confort2, Confort3),
write(Confort3).

% updateKM è n predicato che incrementa/decrementa punteggio in base  ai km effettuati dalla macchina, indica lo stato di degrado del motore
updateKM(KM,PT, PT1):-
   KM>200000,
   PT1 is PT - 0.8.
updateKM(KM,PT, PT1):-
   KM<199999,
   PT1 is PT + 0.8.

% pregimarche è n predicato che incrementa/decrementa punteggio in base  al costo della assistenza e dei pezzi di ricambio
pregimarca(Marca,P,PT2):-
   ricambicostosi(L),
   member(Marca,L),true,
   PT2 is P - 1.4.
   
pregimarca(_,PT,PT2):-
   PT2=PT.

   % probmotore è n predicato che incrementa/decrementa punteggio in base alla durata media del motore
probmotore(Marca,P,PT2):-
   problemamotore([Marca, KM]),
   PT2 is P - 1.4.

probmotore(_,PT,PT2):-
PT2=PT.
