% Last Modified: Mon Mar 21 09:01:20 2011 (vogel) 

:- unknown(_,trace).
:- consult(gaputilities).

% LEXICON %
% -- adjectives -- %
lex(adj,[],ugly).

% -- conjunctions -- %
lex(conj,[],that).
lex(conj,[],whether).

% -- determinators --%
lex(det,[],the).
lex(det,[],a).
lex(det,[],"sandy's").
lex(det,[],which).

% -- nouns -- %
lex(n,[[num,sg]],man).
lex(n,[[num,pl]],men).
lex(n,[[num,sg]],couch).
lex(n,[[num,sg]],picture).
lex(n,[[num,sg]],nose).
lex(n,[[num,sg]],number).
lex(n,[[num,sg]],cat).
lex(n,[[num,pl]],cats).
lex(n,[[num,sg]],charity).
lex(n,[[num,pl]],dogs).
lex(n,[[num,sg]],person).
lex(n,[[num,pl]],people).
lex(n,[[num,sg]],answer).
lex(n,[[num,sg]],end).

% -- proper nouns --%
lex(pn,[],sal).
lex(pn,[],val).
lex(pn,[],lee).

% -- prepositions -- %
lex(prep,[],on).
lex(prep,[],to).
lex(prep,[],up).
lex(prep,[],of).
lex(prep,[],by).

% -- pronouns -- %
lex(pron,[[type,pers],[num,sg],[case,nom],[pers,1]],i).
lex(pron,[[type,pers],[num,sg],[case,acc]],me).
lex(pron,[[type,pers],[num,sg]],you).
lex(pron,[[type,pers],[num,sg],[case,nom],[pers,3]],he).
lex(pron,[[type,pers],[num,sg],[case,acc]],him).
lex(pron,[[type,pers],[num,sg],[case,nom],[pers,3]],she).
lex(pron,[[type,pers],[num,sg],[case,acc],[pers,3]],her).
lex(pron,[[type,pers],[num,sg],[case,nom],[pers,3]],it).
lex(pron,[[type,pers],[num,pl],[case,nom]],they).
lex(pron,[[type,relat]],who).
lex(pron,[[type,relat]],what).
lex(pron,[[type,relat]],that).

% -- verbs -- %
lex(v,[[trans,ditrans],[aux,noaux]],bet).
lex(v,[[pers,3],[num,sg],[tense,present],[trans,ditrans],[aux,noaux]],bets).

lex(v,[[tense,present],[trans,ditrans],[aux,noaux]],give).
lex(v,[[tense,present],[trans,trans],[aux,noaux]],give).
lex(v,[[pers,3],[num,sg],[tense,present],[trans,ditrans]],gives).
lex(v,[[tense,past],[trans,ditrans],[aux,noaux]],gave).

lex(v,[[tense,present],[num,sg],[aux,aux],[trans,trans]],is).
lex(v,[[tense,present],[num,pl],[aux,aux],[trans,trans]],are).
lex(v,[[tense,past],[num,sg],[aux,aux],[trans,trans]],was).

lex(v,[[tense,past],[aux,noaux]],looked).

lex(v,[[num,pl],[tense,present],[aux,noaux]],sleep).
lex(v,[[pers,1],[num,sg],[tense,present],[aux,noaux]],sleep).
lex(v,[[pers,3],[num,sg],[tense,present],[aux,noaux]],sleeps).

lex(v,[[pers,1],[num,sg],[tense,present],[trans,trans],[aux,noaux]],hate).
lex(v,[[pers,3],[num,sg],[tense,present],[trans,trans],[aux,noaux]],hates).
lex(v,[[tense,past],[trans,trans],[aux,noaux]],hated).

lex(v,[[tense,past],[trans,trans],[aux,noaux]],painted).

lex(v,[[tense,present],[trans,trans],[aux,noaux]],believe).
lex(v,[[pers,3],[num,sg],[tense,present],[trans,trans],[aux,noaux]],believes).
lex(v,[[tense,past],[trans,trans],[aux,noaux]],believed).

lex(v,[[tense,present],[aux,noaux]],think).
lex(v,[[pers,3],[num,sg],[tense,present],[trans,intrans],[aux,noaux]],thinks).
lex(v,[[tense,past],[trans,intrans],[aux,noaux]],thought).

lex(v,[[tense,past],[trans,trans],[aux,noaux]],suggested).

lex(v,[[pers,3],[num,sg],[tense,past],[trans,trans],[aux,noaux]],asks).
lex(v,[[tense,past],[trans,trans],[aux,noaux]],asked).

lex(v,[[tense,past],[aux,noaux],[trans,intrans]],wondered).

lex(v,[[pers,3],[num,sg],[tense,present],[trans,trans],[aux,noaux]],does).

lex(v,[[tense,present],[trans,trans],[aux,noaux]],want).
lex(v,[[pers,3],[num,sg],[tense,present],[trans,trans],[aux,noaux]],wants).

lex(v,[[pers,3],[num,sg],[tense,present],[aux,noaux]],seems).

lex(v,[[tense,present],[trans,trans],[aux,noaux]],know).

lex(v,[[pers,3],[num,sg],[tense,present],[trans,trans],[aux,noaux]],understands).

lex(v,[[pers,3],[num,sg],[tense,present],[trans,trans],[aux,noaux]],sings).

lex(v,[[tense,past],[trans,trans],[aux,noaux]],verified).

lex(v,[[tense,past],[trans,trans],[aux,noaux]],said).

% GRAMMAR %

% -- sentence --%
s(bar,[sbar,Conj,S],FeatS,G0-G) -->
	conj(Conj,_FeatConj),
	s(simp,S,FeatS,G0-G).

s(simp,[s, NP, VP],FeatNP,G-G) -->
	np(NP,FeatNP,G-G),
	vp(VP,FeatVP,G-G),
	{ match(FeatNP,FeatVP,[pers,num]), match(FeatNP,[[case,nom]],[case]) }.

s(simp,[s, NP, S],FeatNP,G-G) -->
	np(NP,FeatNP,G-G),
	s(gap,S,FeatNP,G-G),
	{ match(FeatNP,[[case,nom]],[case]) }.

s(gap,[s, NP, VP],FeatNP,G-G) -->
	np(NP,FeatNP,G-G),
	vp(VP,FeatVP,gap(np)-nogap),
	{ match(FeatNP,FeatVP,[pers,num]), match(FeatNP,[[case,nom]],[case]) }.

s(gap,[s, NP, VP],FeatS,G-G) -->
	np(NP,_FeatNP,gap(np)-nogap),
	vp(VP,FeatVP,G-G),
	{ match(FeatS,FeatVP,[pers,num]) }.

s(gap,[s, PP, S],FeatS,G-G) -->
	pp(PP,_FeatPP,G-G),
	s(simp,S,FeatS,gap(pp)-nogap).

s(rel,[rels, RelPron, S],FeatS,G0-G) -->
	relpron(RelPron,_FeatRelPron),
	s(simp,S,FeatS,G0-G).

% -- nominal phrases --%
np(T,FeatNP,G-G) -->
	np(simp,T,FeatNP,G-G).

np(T,FeatNP,G-G) -->
	np(pp,T,FeatNP,G-G).

np(T,FeatNP,G0-G) -->
	np(rel,T,FeatNP,G0-G).

np(T,FeatNP,G0-G) -->
	np(gap,T,FeatNP,G0-G).

np(gap,[np, [[]]],[],gap(np)-nogap)-->
	[].

np(simp,[np, Det, N],FeatNP,G-G) -->
	det(Det,FeatDet),
	n(N,FeatN),
	{ append(FeatDet,FeatN,FeatNP) }.

np(simp,[np, N],FeatN,G-G) -->
	n(N,FeatN),
	{ match(FeatN,[[num,pl]],[num]) }.

np(simp,[np, Det, Adj, N],FeatNP,G-G) -->
	det(Det,FeatDet),
	adj(Adj,_FeatAdj),
	n(N,FeatN),
	{ append(FeatDet,FeatN,FeatNP) }.

np(simp,[np, PN],FeatNP,G-G) -->
	pn(PN,FeatNP).

np(simp,[np, PPron],FeatNP,G-G) -->
	ppron(PPron,FeatNP).

np(pp,[np, NP, PP],FeatNP,G0-G) -->
	np(simp,NP,FeatNP,G0-G1),
	pp(PP,_FeatPP,G1-G).

np(rel,[np, NP, SRel],FeatNP,G0-G) -->
	np(simp,NP,FeatNP,G1-G1),
	s(rel,SRel,FeatNP,G0-G).

np(rel,[np, NP, SRel],FeatNP,G0-G) -->
	np(gap,NP,FeatNP,G1-G1),
	s(rel,SRel,FeatNP,G0-G).


% -- prepositional phrase -- %
pp([pp,Prep,NP],_FeatPP,G0-G) -->
	prep(Prep,_FeatPrep),
	np(NP,FeatNP,G0-G),
	{ match(FeatNP,[[case,acc]],[case]) }.

pp([pp, [[]]],[],gap(pp)-nogap) -->
	[].

% -- verbal phrases -- %
vp(T,FeatVP,G0-G) -->
	vp(simp,T,FeatVP,G0-G).

vp(T,FeatVP,G0-G) -->
	vp(pp,T,FeatVP,G0-G).

vp(T,FeatVP,G0-G) -->
	vp(bar,T,FeatVP,G0-G).

vp(simp,[vp, IV, PP],FeatIV,G0-G) -->
	iv(IV,FeatIV),
	pp(PP,_FeatPP,G0-G).

vp(pp,[vp, TV, NP, PP],FeatVP,G0-G)-->
	tv(TV,FeatTV),
	np(NP,FeatNP,G0-G1),
	pp(PP,_FeatPP,G1-G),
	{ append(FeatTV,FeatNP,FeatVP) }.

vp(simp,[vp, TV, NP],FeatVP,G0-G)-->
	tv(TV,FeatTV),
	np(NP,FeatNP,G0-G),
	{ match(FeatNP,[[case,acc]],[case]),append(FeatTV,FeatNP,FeatVP) }.

vp(simp,[vp, DTV, NP1, NP2],FeatVP,G0-G)-->
	dtv(DTV,FeatDTV),
	np(NP1,FeatNP1,G0-G1),
	np(NP2,_FeatNP2,G1-G),
	{ match(FeatNP1,[[case,acc]],[case]),append(FeatDTV,FeatNP1,FeatVP) }.

vp(bar,[vp, TV, NP, S],FeatVP,G0-G)-->
	tv(TV,FeatTV),
	np(NP,FeatNP,G1-G1),
	s(bar,S,_FeatS,G0-G),
	{ match(FeatNP,[[case,acc]],[case]),append(FeatTV,FeatNP,FeatVP) }.

vp(bar,[vp, DTV, NP1, NP2, S],FeatVP,G0-G)-->
	dtv(DTV,FeatDTV),
	np(NP1,FeatNP1,G1-G1),
	np(NP2,_FeatNP2,G2-G2),
	s(bar,S,_FeatS,G0-G),
	{ match(FeatNP1,[[case,acc]],[case]),append(FeatDTV,FeatNP1,FeatVP) }.

vp(simp,[vp, IV],FeatIV,G-G) -->
	iv(IV,FeatIV).

vp(simp,[vp, Aux, VP],FeatVP,G0-G) -->
	aux(Aux,_FeatAux),
	vp(VP,FeatVP,G0-G),
	{ match(FeatVP,[[tense,past]],[tense]) }.

% -- words -- %
pn([pn, PN],FeatLex) --> [PN],
	{ lex(pn,FeatLex,PN) }.

n([n, N],FeatLex) --> [N],
	{ lex(n,FeatLex,N) }.

det([det, Det],FeatLex) --> [Det],
	{ lex(det,FeatLex,Det) }.

adj([adj, Adj],FeatLex) --> [Adj],
	{ lex(adj,FeatLex,Adj) }.

iv([iv, IV],FeatLex) --> [IV],
	{ lex(v,FeatLex,IV),match(FeatLex,[[trans,intrans]],[trans]) }.

tv([tv, TV],FeatLex) --> [TV],
	{ lex(v,FeatLex,TV),match(FeatLex,[[trans,trans]],[trans]) }.

dtv([dtv, DTV],FeatLex) --> [DTV],
	{ lex(v,FeatLex,DTV),match(FeatLex,[[trans,ditrans]],[trans]) }.

aux([aux, Aux],FeatLex) --> [Aux],
	{ lex(v,FeatLex,Aux),match(FeatLex,[[aux,aux]],[aux]) }.

prep([prep,Prep],FeatLex) --> [Prep],
	{ lex(prep,FeatLex,Prep) }.

ppron([ppron,PPron],FeatLex) --> [PPron],
	{ lex(pron,FeatLex,PPron),match(FeatLex,[[type,pers]],[type]) }.

relpron([relpron,RelPron],FeatLex) --> [RelPron],
	{ lex(pron,FeatLex,RelPron),match(FeatLex,[[type,relat]],[type]) }.

conj([conj,Conj],FeatLex) --> [Conj],
	{ lex(conj,FeatLex,Conj) }.
	
% MATCHING OF FEATURE STRUCTURES %
% match(+FeatStruct1,+FeatStruct2,+FeatList,-Result)
match([],[],_):-!.
match(_,_,[]):-!.
match(F1,F2,[Feat|Rest]) :-
	match_feat(F1,F2,Feat),
	match(F1,F2,Rest).
match_feat(F1,F2,Feat):-
	feat_value(F1,Feat,V1),
	feat_value(F2,Feat,V2),!,
	match_value(V1,V2).

feat_value([[Feat,V]|_],Feat,V).
feat_value([_|Rest],Feat,V):-feat_value(Rest,Feat,V).
feat_value([],_,[]).

match_value([],_):-!.
match_value(_,[]):-!.
match_value(V,V).