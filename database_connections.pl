%usa la connseione di defult
%rs(+SQL,-Row)
rs( SQL, Row):-
        fconn(Connection),
        odbc_query(Connection, SQL, Row).

%ritona con tutti i siultati
%rsall(+SQL,-Rows)
rsall(SQL, Rows):-
        findall(Row,rs(SQL,Row), Rows).

%ritona la connessione di default
%fconn(-C)
fconn(C):-
        odbc_connect('swi-prolog', C, [user(root), password(root), alias(autosalone), open(once)]).



prep(SQL, Param, Row,Lala):-
        fconn(Connection),
        odbc_prepare(Connection, SQL, Param,Q),
        odbc_execute(Q,Lala,Row).
