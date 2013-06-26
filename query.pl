
%prende tutti i risultati delle tabella
getable(auto,T,Budget):-
        swritef(S,'SELECT * FROM auto where prezzo<= %t', [Budget]),
        rsall(S,Result),
        normalizza(Result,T).

getable(TableName,T):-
        swritef(S,'SELECT * FROM %t', [TableName]),
        rsall(S,Result),
        normalizza(Result,T).

%prende tutti i risultati delle tabella accessori
getableacc(ID,T):-
        swritef(S,'SELECT * FROM accessoriprof where idauto= %t', [ID]),
        rsall(S,Result),
        normalizza(Result,T).





/*
INSERT INTO table_name (column1, column2, column3,...)
VALUES (value1, value2, value3,...)
 */

realsave(marca, [Marca,Livello]):-
         atom_number(Livello,LV),
        prep('INSERT INTO marca (Marca, Livello) VALUES (?,?)',
             [varchar,integer],_,
             [Marca, LV]).

realsave(colore, [Colore, Famiglia]):-
        prep('INSERT INTO colore (colore, famiglia) VALUES (?,?)',
             [varchar,varchar],_,
             [Colore, Famiglia]).
             
realsave(accessori, [Accessori, Confort]):-
        prep('INSERT INTO accessori (accessori, confort) VALUES (?,?)',
             [varchar,varchar],_,
             [Accessori, Confort]).
             

realsave(auto, [Colore,Marca,TP, Modello,DataImmatricolazione,Kilometri,CL,Prezzo]):-
         color_convert(Colore, CO),
         marca_convert(Marca,MA),
         dateconvert(DataImmatricolazione, DI),
         atom_number(Kilometri,KM),
         atom_number(CL,Cilindrata),
         atom_number(Prezzo,PZ),
        prep('INSERT INTO auto (Modello, Marca, DataImmatricolazione, KM, Tipo, Cilindrata, Prezzo, Colore) VALUES (?,?,?,?,?,?,?,?)',
             [varchar,integer,date,integer, varchar, integer, integer, integer],_,
             [Modello, MA, DI, KM, TP, Cilindrata, PZ, CO]).





/*
Query UPDATE
*/

trealsave(auto, [Modello,Marca,DataImmatricolazione,KM,Cilindrata, Tipo, Prezzo, Colore],ID):-
        dateconvert(DataImmatricolazione, DB),
        marca_convert(Marca, MA),
        color_convert(Colore,C),
        prep('UPDATE auto SET Modello=?, Marca=?, DataImmatricolazione=?, KM=?, Tipo=?, Cilindrata=?, Prezzo=?, Colore=? WHERE id = ?',
             [varchar,integer,date, integer, varchar, integer, integer, integer, integer],_,
             [Modello,MA,DB,KM,Tipo, Cilindrata,Prezzo,C,ID]).

trealsave(colore,[C,F],ID):-
        prep('UPDATE colore SET colore = ?, famiglia = ? WHERE id = ?', [varchar, varchar,integer],_,[C,F,ID]).

trealsave(accessori,[C,F],ID):-
        prep('UPDATE accessori SET accessori = ?, confort = ? WHERE id = ?', [varchar, varchar,integer],_,[C,F,ID]).

trealsave(marca,[F,C],ID):-
        prep('UPDATE marca SET Marca = ?, Livello = ? WHERE id = ?', [varchar, integer,integer],_,[C,F,ID]).
        


%utilizzata per i convert
getdata(X, ID, D):-
        swritef(S,'SELECT * FROM %t WHERE id = ?',[X]),
        prep(S,[integer],D, [ID]).
        
%utilizzata per associare gli accessori nella fase di initial data
getdata(A, D):-
prep('SELECT confort FROM accessori WHERE accessori = ?',[varchar],D, [A]).


%predicato per convertire l'id con il loro leggittimo valore
color_convert(HR,ID):-
        prep('SELECT id FROM colore WHERE colore = ?', [varchar],row(ID), [HR]),!.
marca_convert(HZ,ID):-
        prep('SELECT id FROM marca WHERE Marca = ?', [varchar],row(ID), [HZ]),!.
accessori_convert(HZ,ID):-
        prep('SELECT id FROM accessori WHERE accessori = ?', [varchar],row(ID), [HZ]),!.
        
        
%predicato per cancellare dal db
delete(ID,Table):-
        swritef(S,'DELETE FROM %t WHERE id = ?', [Table]),
        prep(S, [integer], _ , [ID]).

%predicato per inserire nel db gli accessori asccociati a una macchina
realelevate(ID,T):-
accessori_convert(T,MA),
     prep('INSERT INTO accessoriprof (idauto, lista) VALUES (?,?)', [integer, integer], _, [ID,MA]).



