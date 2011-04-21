% Last Modified: Mon Mar 21 09:00:37 2011 (vogel) 

:- unknown(_,trace).
:- consult(gaptestsuite).

doone(N) :- test(N,X),drawtree(X).
doone(N,t) :- trace,test(N,X),drawtree(X),notrace.
doone(N,nt) :- notrace,test(N,X),drawtree(X).

pass([]).
pass([H|T]) :-
        test(H,P),drawtree(P),
        write('successfully passed '),write(H),!,nl,
        pass(T).
pass([H|T]) :-
        write('unsuccessfully failed '),write(H),!,nl,
        pass(T).

fail([]).
fail([H|T]) :-
        test(H,P),!,drawtree(P),
        write('unsuccessfully passed '),write(H),nl,
        fail(T).
fail([H|T]) :-
        write('successfully failed '),write(H),nl,
        fail(T).

drawtree(Tree):-
        d_tree(Tree, 0).

d_tree([],_).
d_tree(Atom,Indent) :-
	atomic(Atom),
	tab(Indent),
	write(Atom),nl.
d_tree([Q|UOTED],Indent) :-
	integer(Q),
	name(Word,[Q|UOTED]),!,
	tab(Indent),
	write(Word),nl.
d_tree([Mother|Daughters], Indent):-
        nonlist(Mother),!,
        tab(Indent),                            % .. leave a space ..
        write(Mother),                          % .. write mother node
        calcindent(Indent, NewIndent),          % .. calculate indent
        nl,d_daughters(Daughters, NewIndent).   % handle daughters

d_tree([Mother|Daughters], Indent):-
    d_tree(Mother,Indent),
    d_tree(Daughters,Indent).

nonlist(Item) :- functor(Item,X,_Y),X \== '.'.

d_daughters([],_).
d_daughters([First|Rest], Indent):-             % .. otherwise ..
        nonvar(First),
        d_tree(First, Indent),          % .. handle first
        d_daughters(Rest, Indent).              % .. and handles rest

calcindent(N, N1):-
        N1 is N + 2. 

complement_structured(Subcat,Complement,np(Complement,N,P,C)) :-
	member(np(Complement,N,P,C),Subcat).
complement_structured(Subcat,Complement,Term) :-
	member(Element,Subcat),
	Element =.. [Category|Arguments],
	Category \== np,
	append([Category|Arguments],[Complement],Object),
	Term =.. Object.

% note -- built in in current swi prolog
%tab(0).
%tab(N) :-
%        N > 0,
%        M is N - 1,
%        put_char(' '),
%        tab(M).



% note -- built in in current sicstus
%append([],L,L).
%append([H|T],L,[H|L1]):-
%	append(T,L,L1).

% note -- built in in current sicstus
%member(X,[X|_]).
%member(X,[_|Y]) :-
%	member(X,Y).

pass2([]).
pass2([H|T]) :-
	test(H,_P),!,
	%write('successfully passed '),write(H),!,nl,
        pass2(T).
pass2([H|T]) :-
        write('unsuccessfully failed '),write(H),!,nl,
        pass2(T).

fail2([]).
fail2([H|T]) :-
        test(H,_P),!,
        write('unsuccessfully passed '),write(H),nl,
        fail2(T).
fail2([_|T]) :-
        fail2(T).