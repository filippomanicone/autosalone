:- use_module(library(tabular)).
% :- use_module(library(autowin)).

%   liste delle categorie di vetture
listacategorie([[berlina], [fuoristrada], [suv], [monovolume], [citycar]]).



%prende la lista e la mette in tabella con i pulsanti edit delete e show
fill_table(_,_,[],_).
fill_table(Table,TableID,[D|Ds],[A|IDs]):-
        append_row(Table,D),
        send(Table, append, new(_, button('Visualizza', message(@prolog, show, normal,A, TableID)))),
        send(Table, append, new(_, button('Edita', message(@prolog, show,edit,A, TableID)))),
        send(Table, append, new(_, button('Cancella', message(@prolog, delete,A, TableID)))),
        send(Table,next_row),
        fill_table(Table,TableID,Ds,IDs).


%Tabella con pulsante delete (solo in listaaccesori)
fill_tableacc(_,_,[],_).
fill_tableacc(Table,TableID,[D|Ds],[A|IDs]):-
        append_row(Table,D),
        send(Table, append, new(_, button('Cancella', message(@prolog, delete,A, TableID)))),
        send(Table,next_row),
        fill_tableacc(Table,TableID,Ds,IDs).

%per ogni elemento della lista lo mette in una cella  e trasformat la data
append_row(_,[]).
append_row(Table,[date(Y,M,D)|T]):-
        appendall([D,M,Y],Date,'.'),
        append_row(Table, [Date|T]).
append_row(Table, [H|T]):-
        new(Cell, label('label',H)),
        send(Table, append(Cell) ),
        send(Cell,length,10),
        append_row(Table,T).

%predicato per riempire la finestra risultati
fill_tableresult(_,[]).
fill_tableresult(Table,[D|Ds]):-
        append_rowresult(Table,D),
        send(Table,next_row),
        fill_tableresult(Table,Ds).
append_rowresult(_,[]).
append_rowresult(Table, [H|T]):-
        list_string(H, S),
        new(Cell, label('label',S)),
        send(Table, append(Cell) ),
        send(Cell,length,10),
        append_rowresult(Table,T).

% concatena i vari atomi
list_codes([], "").
list_codes([Atom], Codes) :- atom_codes(Atom, Codes).
list_codes([Atom|ListTail], Codes) :-
        atom_codes(Atom, AtomCodes),
    append(AtomCodes, ",", AtomCodesWithComma),
    append(AtomCodesWithComma, ListTailCodes, Codes),
    list_codes(ListTail, ListTailCodes).

% prende lista e la trasforma in stringa
list_string(List, String) :-
    ground(List),
    list_codes(List, Codes),
    atom_codes(String, Codes).
list_string(List, String) :-
    ground(String),
    atom_codes(String, Codes),
    list_codes(List, Codes).
list_string(List, String):-
    String=List.


%appende l'intestazione della tabella
append_row_bold(Table,[]):-
        send(Table,next_row).
append_row_bold(Table,[H|T]):-
        new(Cell, label('label',H,bold)),
        send(Cell, length,10),
        send(Table, append(Cell) ),

        append_row_bold(Table,T).


% visualizza in formato data
appendate(W,Descr, date(Y,M,D),Mode):-
        appendprimo(W,Descr),
        appendall([D,M,Y],Date,'.'),
        appendultimo(W,Date,Mode).
appendate(W,Descr, date(Y,M,D),Mode,X):-
        appendprimo(W,Descr),
        appendall([D,M,Y],Date,'.'),
        appendultimo(W,Date,Mode,X).

% apppende due etichette in visualizza e una etichetta e una text in modifica
appendtouple(W, Descr, Val,Mode):-
        appendprimo(W,Descr),
        appendall(Val,Vals, ', '),
        appendultimo(W,Vals,Mode).
appendtouple(W, Descr, Val,Mode,X):-
        appendprimo(W,Descr),
        appendall(Val,Vals, ', '),
        appendultimo(W,Vals,Mode,X).

appendtouplet(W, Descr, Val,Mode,X,T):-
        appendprimo(W,Descr),
        appendall(Val,Vals, ', '),
        appendultimo(W,Vals,Mode,X,T).

%appende tutti i risultati
appendall([V],V,_).
appendall([V,V_|Vs], Vals,Between):-
        appendall([V_|Vs],Val,Between),
        atom_concat(V,Between, Vcomma),
        atom_concat(Vcomma,Val,Vals).


appendvals(W,[X],Mode):-
        appendultimo(W,X,Mode).
appendvals(W,[Val,Valt|Vals],Mode):-
        appendval(W,Val,Mode),
        appendvals(W, [Valt|Vals],Mode).


appendval(W,Val,_):-
        atom_concat(Val, ', ', Valcomma),
        new(Label, label('meep1',Valcomma)),

        atom_codes(Valcomma, ValL),
        length(ValL,L),
        send(Label, length,L),
        send(W, append(Label, right) ).

appendultimo(W,Val,normal):-
        new(Label, label('',Val)),
        atom_codes(Val, ValL),
        length(ValL,L),
        send(Label, length,L),
        send(W, append(Label,right)).

appendultimo(W,Val,edit):-
        new(Label, text_item('',Val)),
        atom_codes(Val, ValL),
        length(ValL,L),
        send(Label, length,L+2),
        send(W, append(Label,right)).



appendultimo(W,Val,normal,Label):-
        new(Label, label('',Val)),
        atom_codes(Val, ValL), length(ValL,L), send(Label, length,L),
        send(W, append(Label,right)).

appendultimo(W,Val,edit,Label):-
        new(Label, text_item('',Val)),
        send(Label, length,42),
        send(W, append(Label,right)).


appendultimo(W,Val,edit,X,T):-
        new(T, text_item('',Val)),
        send(T, length,X),
        send(W, append(T,right)).


appendprimo(W,Descr):-
        new(Label, label('meep0',Descr,bold)),
        atom_codes(Descr,DescL),
        length(DescL,L),
        send(Label,length,L),
        send(W, append(Label)).



%inserisce bottone

newbuttons(_,_,[],_).
newbuttons(Window,C,[X|Xs],Pot):-
        newbutton(Window,C,X,Pot),
        newbuttons(Window,C,Xs,Pot).
newbutton(Window,C,X,Pot):-
        new(B, button(X, message(@prolog, C,X))),
        send(Window, append(B,Pot) ).

%menu principale
main(Window):-
        newbuttons(Window,viewtable, ['auto','marca', 'colore', 'accessori', domanda],below).

%chiama menu principale
autosalone:-
        new(W,dialog('Autosalone')),
        main(W),
        send(W,open).

%avvia sistema
viewtable(domanda):-
   abolish(textitems/2),
   startengine.

%tabella visualizza
viewtable(X):-
        abolish(textitems/2),
        new(W,dialog(X)),
        new(Label, label('label', X)),
        send(W,append(Label)),
        new(B,button('Aggiungi', message(@prolog, add,X,W))),
        send(W,append(B,right)),
        new(Table,tabular),
        getable(X,RawT),
        getrows(X,RawT,T,ID),
        rowsnames(X,Names),
        append_row_bold(Table,Names),
        fill_table(Table, X,T,ID),
        send(W, append(Table,below)),
        send(W,open).



getrows(_,[],[],[]).
getrows(X,[R|Rs], [T|Ts],[ID|IDs]):-
        enchancerows(X,R,T,ID),
        getrows(X,Rs,Ts,IDs).

enchancerows(auto, [N,S,DB,KM,Tipo,_,Prezzo,CO,ID], [N,DB,KM,Tipo,Marca,Prezzo,Colore],ID):-
        getdata(marca,S, row(_, Marca,_)),
        getdata(colore, CO, row(_,Colore,_)).
enchancerows(colore, [ID,A,B], [A,B],ID).
enchancerows(marca, [ID,A,B], [A,B],ID).
enchancerows(accessori, [ID,A,B], [A,B],ID).
enchancerows(accessoriprof, [ID,A,_], [Accessori],ID):-
        getdata(accessori, A, row(_,Accessori,_)).

%  Intestazione Tabella
rowsnames(auto, ['Modello', 'Data Immatricolazione', 'KM', 'Tipo','Marca', 'Prezzo','Colore']).
rowsnames(colore, ['Colore', 'Famiglia']).
rowsnames(marca, ['Marca', 'Livello']).
rowsnames(accessori, ['Accessori', 'Confort']).
rowsnames(accessoriprof, ['Accessori']).
rowsnames(risultati, ['ID','Modello', 'Data Immatricolazione', 'KM', 'Tipo','Marca', 'Prezzo','Colore','Acessorio','Punteggio']).



%  Normalizza i risultati della query ognuno per ogni tipo di lunghezza
normalizza([],[]).
normalizza([row(X)|Rs], [[X]|Xs]):-
        normalizza(Rs,Xs).
normalizza([row(X,Y)|Rs], [[X,Y]|Xs]):-
        normalizza(Rs,Xs).
normalizza([row(X,Y,Z)|Rs], [[X,Y,Z]|Xs]):-
        normalizza(Rs,Xs).
normalizza([row(X,Y,Z,A)|Rs], [[X,Y,Z,A]|Xs]):-
        normalizza(Rs,Xs).
normalizza([row(X,Y,Z,A,B)|Rs], [[X,Y,Z,A,B]|Xs]):-
        normalizza(Rs,Xs).
normalizza([row(X,Y,Z,A,B,C)|Rs], [[X,Y,Z,A,B,C]|Xs]):-
        normalizza(Rs,Xs).
normalizza([row(X,Y,Z,A,B,C,X2,Y2,Z2)|Rs], [[X,Y,Z,A,B,C,X2,Y2,Z2]|Xs]):-
        normalizza(Rs,Xs).
normalizza([row(X,Y,Z,A,B,C,X2,Y2,Z2,A2,B2,C2,VV,CC)|Rs], [[X,Y,Z,A,B,C,X2,Y2,Z2,A2,B2,C2,VV,CC]|Xs]):-
        normalizza(Rs,Xs).



 appendadd(W,X,Y,T):-
         appendtouplet(W,X,[''],edit,Y,T).
appendadd(W,X,T):-
        appendadd(W,X,42,T).
appendadds(_,[],[]).
appendadds(W,[X|Xs],[T|Ts]):-
        appendadd(W,X,T),
        appendadds(W,Xs,Ts).

%finestra aggiungi nuovo
add(X,W):-
        add(X),
        free(W).
add(X):-
        new(W,dialog('Aggiungi')),
        addata(W,X,T),
        assert( textitems(W,T) ),

        new(B,button('Salva', message(@prolog, save,W,X))),
        send(W,append(B,below)),
        send(W,open).

getvalues([],[]).
getvalues([T|Ts], [V|Vs]):-
        get(T,selection,V),
        getvalues(Ts,Vs).



save(W,X):-
        textitems(W,T),
        getvalues(T,V),
        realsave(X,V),
        free(W).




addata(W,auto,[T1,T2,T3|Ts]):-
        appendadds(W,['Modello', 'Data immatricolazione', 'KM', 'Cilindrata', 'Prezzo'],Ts),
         dropdowncolore(W,'Colore',T1),
         dropdownmarca(W,'Marca',T2),
         dropdowntipo(W,'Tipo',T3).

addata(W,colore,C):-
        appendadds(W,['Colore','Famiglia'],C).
addata(W,marca,T):-
        appendadds(W,['Marca','Livello'],T).
addata(W,accessori,C):-
        appendadds(W,['Accessori','Confort'],C).
addata(W,domanda,C):-
        appendadds(W,['Inserisci numero'],C).

getaccessorilist([],[]).
getaccessorilist([ [_,Accessori,_]|Rs], [X|Ts]):-
        getaccessorilist(Rs,Ts),
        appendall([Accessori],X,', ').

getcolorelist([],[]).
getcolorelist([ [_,Colore,_]|Rs], [X|Ts]):-
        getcolorelist(Rs,Ts),
        appendall([Colore],X,', ').
getmarcalist([],[]).
getmarcalist([ [_,Marca,_]|Rs], [X|Ts]):-
        getmarcalist(Rs,Ts),
        appendall([Marca],X,', ').
gettipolist([],[]).
gettipolist([ [Tipo]|Rs], [X|Ts]):-
        gettipolist(Rs,Ts),
        appendall([Tipo],X,', ').

% combobox colore
dropdowncolore(W,Des,D):-
        getable(colore,RT),
        getcolorelist(RT,T),
        send(W, append(new(_,label('la',Des,bold)))),
        new(D, menu('',cycle)),
        send(W,append,D,right),
        send_list(D,append,T).

% combobox accessori
dropdownaccessori(W,Des,D):-
        getable(accessori,RT),
        getaccessorilist(RT,T),
        send(W, append(new(_,label('la',Des,bold)))),
        new(D, menu('',cycle)),
        send(W,append,D,right),
        send_list(D,append,T).

% combobox colore
dropdowncolore(W,Des,D,First):-
        getable(colore,RT),
        getcolorelist(RT,T),
        select(First,T,Rest),
        send(W, append(new(_,label('la',Des,bold)))),
        new(D, menu('',cycle)),
        send(W,append,D,right),
        send_list(D,append,[First|Rest]).

% combobox marca
dropdownmarca(W,Des,D):-
        getable(marca,RT),
        getmarcalist(RT,T),
        send(W, append(new(_,label('la',Des,bold)))),
        new(D, menu('',cycle)),
        send(W,append,D,right),
        send_list(D,append,T).

dropdownmarca(W,Des,D,First):-
       getable(marca,RT),
        getmarcalist(RT,T),
        select(First,T,Rest),
        send(W, append(new(_,label('la',Des,bold)))),
        new(D, menu('',cycle)),
        send(W,append,D,right),
        send_list(D,append,[First|Rest]).

% combobox tipo
dropdowntipo(W,Des,D):-
listacategorie(RT),
        gettipolist(RT,T),
        send(W, append(new(_,label('la',Des,bold)))),
        new(D, menu('',cycle)),
        send(W,append,D,right),
        send_list(D,append,T).

dropdowntipo(W,Des,D,First):-
        listacategorie(RT),
        gettipolist(RT,T),
        select(First,T,Rest),
        send(W, append(new(_,label('la',Des,bold)))),
        new(D, menu('',cycle)),
        send(W,append,D,right),
        send_list(D,append,[First|Rest]).

%converte il fromato della data
dateconvert(HR,date(Dn,Mn,Yn)):-
        atom_concat(Y,Xya,HR),
        atom_concat('.',Xyb,Xya),
        atom_concat(M,Xma,Xyb),
        atom_concat('.', D,Xma),
        atom_number(Y,Yn),
        atom_number(M,Mn),
        atom_number(D,Dn).



show(M,ID,T):-
        new(W,dialog(T)),
        showbasic(W,M,T,ID),
        modespec(W,M,T,ID),
        send(W,open).

modespec(W,edit,T,ID):-
        new(B,button('save', message(@prolog, update,W,T,ID))),
        send(W,append(B)).
modespec(_,normal,_,_).


showbasic(W,M,auto,ID):-
        morphdata(auto,ID,D),
        auto(W,M,D).

showbasic(W,M,colore,ID):-
        prep('SELECT * FROM colore WHERE id = ?', [integer], row(_,N,C),[ID]),
        appendtouple(W,'Colore',[N],M,AA),
        appendtouple(W, 'Famiglia:', [C],M,BB),
        assert(textitems(W,[AA,BB])).
showbasic(W,M,accessori,ID):-
        prep('SELECT * FROM accessori WHERE id = ?', [integer], row(_,N,C),[ID]),
        appendtouple(W,'Accessori',[N],M,AA),
        appendtouple(W, 'Confort:', [C],M,BB),
        assert(textitems(W,[AA,BB])).


showbasic(W,M,marca,ID):-
        prep('SELECT * FROM marca WHERE id = ?', [integer], row(_,N,C),[ID]),
        appendtouple(W,'Marca',[N],M,AA),
        appendtouple(W, 'Livello:', [C],M,BB),
        assert(textitems(W,[AA,BB])).

auto(Window,Mode,[ID, Modello, Marca, DataImmatricolazione, Km, Tipo, Cilindrata, Prezzo,Colore]):-
        send(Window, alignment,center),
        new(Space,label('space','')),
        send(Window,append(Space,below)),
        appendtouple(Window, 'Modello:', [Modello],Mode,AA),
        appendmarca(Window, 'Marca:', [Marca],Mode,BB),
        appendate(Window,'Data immatricolazione', DataImmatricolazione,Mode,CC),
        appendtouple(Window, 'KM', [Km],Mode,DD),
        appendtouple(Window, 'Cilindrata', [Cilindrata],Mode,EE),
        appendtipo(Window, 'Tipo', [Tipo],Mode,FF),
        appendtouple(Window, 'Prezzo:', [Prezzo],Mode,GG),
        appendcolore(Window, 'Colore:', [Colore],Mode,HH),
        assert(textitems(Window,[AA,BB,CC,DD, EE, FF,GG,HH])),
        send(Window, append, new(_, button('Aggiungi Accessori', message(@prolog, accessori,ID)))).

appendaccessori(Window, Des, [Accessori],normal,HH):-
        appendtouple(Window, Des, [Accessori],normal,HH).
appendcolore(Window, Des, [Colore],normal,HH):-
        appendtouple(Window, Des, [Colore],normal,HH).

appendcolore(Window, Des, List,edit,HH):-
        appendall(List,First,', '),
        dropdowncolore(Window,Des,HH,First).

appendmarca(Window, Des, [Marca],normal,HH):-
        appendtouple(Window, Des, [Marca],normal,HH).
appendmarca(Window, Des, List,edit,HH):-
        appendall(List,First,', '),
        dropdownmarca(Window,Des,HH,First).

appendtipo(Window, Des, [Marca],normal,HH):-
        appendtouple(Window, Des, [Marca],normal,HH).
appendtipo(Window, Des, List,edit,HH):-
        appendall(List,First,', '),
        dropdowntipo(Window,Des,HH,First).

morphdata(auto,ID,D):-
        getdata(auto, ID,
        row(Modello, Marca1, DataImmatricolazione, KM, Tipo, Cilindrata, Prezzo, Colore1,_) ), %ProfID removed
        getdata(colore, Colore1, row(_,Colore,_)),
        getdata(marca, Marca1, row(_,Marca,_)),
        D = [ID, Modello, Marca, DataImmatricolazione, KM, Tipo, Cilindrata, Prezzo, Colore].

 accessori(IDX):-
        new(W,dialog(accessori)),
        new(Label, label('label', accessori)),
        send(W,append(Label)),
        new(Table,tabular),
        getableacc(IDX,RawT),
        getrows(accessoriprof,RawT,T,ID),
        rowsnames(accessoriprof,Names),
        append_row_bold(Table,Names),
        fill_tableacc(Table, accessoriprof,T,ID),
        send(W, append(Table,below)),
        dropdownaccessori(W,'Accessori',T1),
        assert(textitems(W,[T1])),
        send(W, append, new(_, button('save', message(@prolog, elevate,IDX,W)))),
        send(W,open).




elevate(ID,W):-
     textitems(W,T),
     getvalues(T,[A]),
     realelevate(ID,A).

normalizet([],[]).
normalizet([row(X,_,_)|T],[X|L]):-
        normalizet(T,L).


normalizetid([],[]).
normalizetid([row(_,_,X)|T],[X|L]):-
        normalizetid(T,L).


update(W,Table,ID):-
        textitems(W,T),
        getvalues(T,V),
        trealsave(Table,V,ID),
        free(W).



% interfaccia per porre domande all'utente la prima Ë utilizza per la risposta libera, altra
% Ë utilizzata per le risposte in un set di valori
user(Question,X,Goal):-
        new(W,dialog('Domanda')),
        appendprimo(W,Question),
        new(M, menu('')),
        send_list(M,append,X),
        send(W,append(M,right)),
        assert(textitems(W,[M])),
        send(W, append, new(_, button('continua', message(@prolog, risposta,W,Goal)))),
        send(W,open).

user(Question,Goal):-
        new(W,dialog('Domanda')),
        appendprimo(W,Question),
        addata(W,domanda,M),
        assert(textitems(W,M)),
        send(W, append, new(_, button('continua', message(@prolog, risposta,W,Goal)))),
        send(W,open).



% prende la risposta e attiva la regola
risposta(W,Goal):-
      textitems(W,T),
      getvalues(T,[A]),
      regola(Goal,A),
      free(W).

 results:-
        new(W,dialog('Risultati')),
        new(Label, label('label', 'Risultati')),
        send(W,append(Label)),
        new(Table,tabular),
        rowsnames(risultati,Names),
        append_row_bold(Table,Names),
        findall(L,automobile(L),ListaAutomobile),
        ordinarisultati(ListaAutomobile,ListaAutomobil),
        fill_tableresult(Table, ListaAutomobil),
        send(W, append(Table,below)),
        send(W,open).


    ordinarisultati(Lista, Sorted) :-
    predsort(compara, Lista, Sorted).
    

    compara(R, X, Y) :-
    last(X, Xl),
    last(Y, Yl),
    ( Xl > Yl -> R = (<) ; R = (>) ).
