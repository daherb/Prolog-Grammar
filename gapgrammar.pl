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
lex(ppron,[[num,sg],[case,nom],[pers,1]],i).
lex(ppron,[[num,sg],[case,acc]],me).
lex(ppron,[[num,sg]],you).
lex(ppron,[[num,sg],[case,nom],[pers,3]],he).
lex(ppron,[[num,sg],[case,acc]],him).
lex(ppron,[[num,sg],[case,nom],[pers,3]],she).
lex(ppron,[[num,sg],[case,acc],[pers,3]],her).
lex(ppron,[[num,sg],[case,nom],[pers,3]],it).
lex(ppron,[[num,pl],[case,nom]],they).
lex(relpron,[],who).
lex(relpron,[],what).
lex(relpron,[],that).

% -- verbs -- %
lex(attv,[[tense,past]],believed).
lex(attv,[[tense,present]],know).
lex(attv,[[pers,3],[num,sg],[tense,present]],seems).
lex(attv,[[tense,past]],suggested).
lex(attv,[[tense,past]],thought).

lex(auxv,[[tense,present],[num,sg]],is).
lex(auxv,[[tense,present],[num,pl]],are).
lex(auxv,[[tense,past],[num,sg]],was).

lex(dtv,[[tense,past]],believed).
lex(dtv,[[trans,ditrans]],bet).
lex(dtv,[[pers,3],[num,sg],[tense,present]],bets).
lex(dtv,[[tense,present]],give).
lex(dtv,[[pers,3],[num,sg],[tense,present]],gives).
lex(dtv,[[tense,past]],gave).
lex(dtv,[[tense,past]],thought).

lex(iv,[[tense,past]],looked).
lex(iv,[[tense,past]],said).
lex(iv,[[num,pl],[tense,present]],sleep).
lex(iv,[[pers,1],[num,sg],[tense,present]],sleep).
lex(iv,[[pers,3],[num,sg],[tense,present]],sleeps).
lex(iv,[[tense,present]],think).
lex(iv,[[pers,3],[num,sg],[tense,present]],thinks).
lex(iv,[[tense,past]],thought).
lex(iv,[[tense,past]],wondered).

lex(tv,[[pers,3],[num,sg],[tense,past]],asks).
lex(tv,[[tense,past]],asked).
lex(tv,[[tense,present]],believe).
lex(tv,[[pers,3],[num,sg],[tense,present]],believes).
lex(tv,[[tense,past]],believed).
lex(tv,[[pers,3],[num,sg],[tense,present]],does).
lex(tv,[[num,pl],[tense,present]],hate).
lex(tv,[[pers,3],[num,sg],[tense,present]],hates).
lex(tv,[[tense,past]],hated).
lex(tv,[[tense,present],[num,sg]],is).
lex(tv,[[tense,present],[num,pl]],are).
lex(tv,[[tense,past],[num,sg]],was).
lex(tv,[[tense,present]],know).
lex(tv,[[tense,past]],painted).
lex(tv,[[pers,3],[num,sg],[tense,present]],sings).
lex(tv,[[tense,past]],suggested).
lex(tv,[[pers,3],[num,sg],[tense,present]],understands).
lex(tv,[[tense,present]],want).
lex(tv,[[tense,past]],verified).
lex(tv,[[pers,3],[num,sg],[tense,present]],wants).

% GRAMMAR %

% -- sentence --%
s(simp,[s1, NP, VP],_FeatS,G0-G) -->
	np(NP,FeatNP,G0-G),
	vp(VP,FeatVP,_G1-_G2),
	{ match(FeatNP,[[case,nom]|FeatVP],[case,num,pers]) }.

s(simp,[s2, NP, S],_FeatS,G-G) -->
	np(NP,FeatNP,G-G),
	s(gap,S,FeatNP,gap(np)-nogap).

s(simp,[s3, PP, S],_FeatS,G-G) -->
	pp(PP,_FeatPP,G-G),
	s(gap,S,_FeatS2,gap(pp)-nogap).

s(simp,[s4,S,VP],FeatS,G-G) -->
	s(rel,S,FeatS,gap(np)-nogap),
	vp(VP,FeatVP,G-G),
	{ match(FeatS,[[case,nom]|FeatVP],[case,num,pers]) }.

s(gap,[sgap1, NP, VP],_FeatS,gap(G)-nogap) -->
	np(NP,FeatNP,G0-G0),
	vp(VP,FeatVP,gap(G)-nogap),
	{ match(FeatNP,[[case,nom]|FeatVP],[case,num,pers]) }.

s(gap,[sgap2, NP, VP],FeatS,gap(np)-nogap) -->
	np(NP,_FeatNP,gap(np)-nogap),
	vp(VP,FeatVP,G-G),
	{ match(FeatS,[[case,nom]|FeatVP],[case,num,pers]) }.
	
s(bar,[sbar1, Conj, S],FeatS,G-G) -->
	conj(Conj,_FeatConj),
	s(simp,S,FeatS,G-G).

s(bar,[sbar2, Conj, S],FeatS,G0-G) -->
	conj(Conj,_FeatConj),
	s(gap,S,FeatS,G0-G).

s(rel,[srel1,RelPron, S],FeatS,G0-G) -->
	relpron(RelPron,_FeatRelPron),
	s(simp,S,FeatS,G0-G).

s(rel,[srel2,RelPron, S],FeatS,G0-G) -->
	relpron(RelPron,_FeatRelPron),
	s(gap,S,FeatS,G0-G).

s(inf,[sinf1, NP, VP],_FeatS,G0-G) -->
	np(NP,FeatNP,G0-G),
	vp(inf,VP,FeatVP,_G1-_G2),
	{ match(FeatNP,[[case,nom]|FeatVP],[case,num,pers]) }.

% -- nominal phrases --%
np(T,FeatNP,G-G) -->
	np(simp,T,FeatNP,G-G).

np(T,FeatNP,G-G) -->
	np(pp,T,FeatNP,G-G).

np(T,FeatNP,G0-G) -->
	np(rel,T,FeatNP,G0-G).

np(T,FeatNP,G0-G) -->
	np(gap,T,FeatNP,G0-G).

np(gap,[npgap1, [[]]],[],gap(np)-nogap)-->
	[].

np(simp,[np1, PN],FeatNP,G-G) -->
	pn(PN,FeatNP).

np(simp,[np2, PPron],FeatNP,G-G) -->
	ppron(PPron,FeatNP).

np(simp,[np3, N],FeatN,G-G) -->
	n(N,FeatN),
	{ match(FeatN,[[num,pl]],[num]) }.


np(simp,[np4, Det, N],FeatNP,G-G) -->
	det(Det,FeatDet),
	n(N,FeatN),
	{ append(FeatDet,FeatN,FeatNP) }.

np(simp,[np5, Det, Adj, N],FeatNP,G-G) -->
	det(Det,FeatDet),
	adj(Adj,_FeatAdj),
	n(N,FeatN),
	{ append(FeatDet,FeatN,FeatNP) }.

np(pp,[nppp1, NP, PP],FeatNP,G0-G) -->
	np(simp,NP,FeatNP,G0-G1),
	pp(PP,_FeatPP,G1-G).

np(rel,[nprel1, NP, SRel],FeatNP,G0-G) -->
	np(simp,NP,FeatNP,G1-G1),
	s(rel,SRel,FeatNP,G0-G).

np(rel,[nprel2, NP, SRel],FeatNP,G0-G) -->
	np(gap,NP,FeatNP,G0-G1),
	s(rel,SRel,FeatNP,G1-G).

% -- prepositional phrase -- %
pp([pp1,Prep,NP],_FeatPP,G0-G) -->
	prep(Prep,_FeatPrep),
	np(NP,FeatNP,G0-G),
	{ match(FeatNP,[[case,acc]],[case]) }.

pp([pp2, [[]]],[],gap(pp)-nogap) -->
	[].

% -- verbal phrases -- %
vp(T,FeatVP,G0-G) -->
	vp(simp,T,FeatVP,G0-G).

vp(T,FeatVP,G0-G) -->
	vp(pp,T,FeatVP,G0-G).

vp(T,FeatVP,G0-G) -->
	vp(bar,T,FeatVP,G0-G).

vp(T,FeatVP,G0-G) -->
	vp(att,T,FeatVP,G0-G).

vp(simp,[vp1, IV, PP],FeatIV,G0-G) -->
	iv(IV,FeatIV),
	pp(PP,_FeatPP,G0-G).

vp(pp,[vppp1, TV, NP, PP],FeatVP,G0-G)-->
	tv(TV,FeatTV),
	np(NP,FeatNP,G0-G1),
	pp(PP,_FeatPP,G1-G),
	{ append(FeatTV,FeatNP,FeatVP) }.

vp(pp,[vppp2, TV, NP, PP],FeatVP,G0-G)-->
	dtv(TV,FeatTV),
	np(NP,FeatNP,G0-G1),
	pp(PP,_FeatPP,G1-G),
	{ append(FeatTV,FeatNP,FeatVP) }.

vp(pp,[vppp3, TV, PP],FeatTV,nogap-nogap)-->
	tv(TV,FeatTV),
	pp(PP,_FeatPP,nogap-nogap).

vp(simp,[vp2, TV, NP],FeatVP,G0-G)-->
	tv(TV,FeatTV),
	np(NP,FeatNP,G0-G),
	{ match(FeatNP,[[case,acc]],[case]),append(FeatTV,FeatNP,FeatVP) }.

vp(simp,[vp3, DTV, NP1, NP2],FeatVP,G0-G)-->
	dtv(DTV,FeatDTV),
	np(NP1,FeatNP1,G0-G1),
	np(simp,NP2,_FeatNP2,G1-G),
	{ match(FeatNP1,[[case,acc]],[case]),append(FeatDTV,FeatNP1,FeatVP) }.

vp(simp,[vp4, IV],FeatIV,G-G) -->
	iv(IV,FeatIV).

vp(simp,[vp5, Aux, VP],FeatVP,G0-G) -->
	auxv(Aux,_FeatAux),
	vp(VP,FeatVP,G0-G),
	{ match(FeatVP,[[tense,past]],[tense]) }.

vp(bar,[vpbar1, TV, NP, S],FeatVP,G0-G)-->
	tv(TV,FeatTV),
	np(NP,FeatNP,G1-G1),
	s(bar,S,FeatNP,G0-G),
	{ match(FeatNP,[[case,acc]],[case]),append(FeatTV,FeatNP,FeatVP) }.

vp(bar,[vpbar2, DTV, NP1, NP2, S],FeatVP,G-G)-->
	dtv(DTV,FeatDTV),
	np(NP1,FeatNP1,G1-G1),
	np(NP2,_FeatNP2,G2-G2),
	s(bar,S,FeatNP1,G-G),
	{ match(FeatNP1,[[case,acc]],[case]),append(FeatDTV,FeatNP1,FeatVP) }.

vp(att,[vpatt1, AttV,S],FeatVP,G-G) -->
	attv(AttV,FeatVP),
	s(simp,S,_FeatS,G-G).

vp(att,[vpatt1, AttV,S],FeatVP,G-G) -->
	attv(AttV,FeatVP),
	s(inf,S,_FeatS,G-G).

vp(inf,[vpinf1, to, InfV, NP],FeatVP,G-G) -->
	to,
	inftv(InfV,FeatVP),
	np(NP,FeatNP,G-G),
	{ match(FeatNP,[[case,acc]],[case]) }.

% -- words -- %
to --> [to].

pn([pn, PN],FeatLex) --> [PN],
	{ lex(pn,FeatLex,PN) }.

n([n, N],FeatLex) --> [N],
	{ lex(n,FeatLex,N) }.

det([det, Det],FeatLex) --> [Det],
	{ lex(det,FeatLex,Det) }.

adj([adj, Adj],FeatLex) --> [Adj],
	{ lex(adj,FeatLex,Adj) }.

iv([iv, IV],FeatLex) --> [IV],
	{ lex(iv,FeatLex,IV) }.

tv([tv, TV],FeatLex) --> [TV],
	{ lex(tv,FeatLex,TV) }.

dtv([dtv, DTV],FeatLex) --> [DTV],
	{ lex(dtv,FeatLex,DTV) }.

attv([attv, AttV],FeatLex) --> [AttV],
	{ lex(attv,FeatLex,AttV) }.

inftv([infdtv, InfV],FeatLex) --> [InfV],
	{ lex(tv,FeatLex,InfV),match(FeatLex,[[num,pl]],[num]) }.
	
auxv([auxv, Aux],FeatLex) --> [Aux],
	{ lex(auxv,FeatLex,Aux) }.

prep([prep,Prep],FeatLex) --> [Prep],
	{ lex(prep,FeatLex,Prep) }.

ppron([ppron,PPron],FeatLex) --> [PPron],
	{ lex(ppron,FeatLex,PPron) }.

relpron([relpron,RelPron],FeatLex) --> [RelPron],
	{ lex(relpron,FeatLex,RelPron) }.

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