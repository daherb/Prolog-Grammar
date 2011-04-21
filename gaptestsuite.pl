% Last Modified: Mon Mar 21 09:01:41 2011 (vogel) 

:- unknown(_,trace).
:- use_module(library(terms)).


test(1,X) :-    % pass V
	s(_Type,X,_B,G-G,[the,man,sleeps,on,the,couch],[]).
test(a1,X) :-   % pass V
	s(_Type,X,_B,G-G,[the,man,sleeps,on,the,ugly,couch],[]).
test(b1,X) :-   % pass V
	s(_Type,X,_B,G-G,[the,couch,the,man,sleeps,on],[]).
test(c1,X) :-   % pass X
	s(_Type,X,_B,G-G,[on,the,couch,the,man,sleeps],[]).
test(d1,X) :-   % pass V
	s(_Type,X,_B,G-G,[the,ugly,couch,the,man,sleeps,on],[]).
test(e1,X) :-   % pass X
	s(_Type,X,_B,G-G,[on,the,ugly,couch,the,man,sleeps],[]).
test(f1,X) :-   % fail V
	s(_Type,X,_B,G-G,[couch,the,man,sleeps,on,the,ugly],[]). %no

test(2,X) :-    % pass V
	s(_Type,X,_B,G-G,[the,men,sleep,on,the,couch],[]).
test(3,X) :-    % fail V
	s(_Type,X,_B,G-G,[the,man,sleep,on,the,couches],[]).
test(4,X) :-    % fail V
	s(_Type,X,_B,G-G,[the,men,sleeps,on,the,couches],[]).
test(5,X) :-    % pass V
	s(_Type,X,_B,G-G,[the,man,sleeps],[]).
test(6,X) :-    % fail V
	s(_Type,X,_B,G-G,[she,gives,the, couch,to,i],[]).
test(7,X) :-    % pass V
	s(_Type,X,_B,G-G,[she,gives,the, couch,to,me],[]).
test(8,X) :-    % fail V
	s(_Type,X,_B,G-G,[her,gives,the, couch,to,me],[]).
test(9,X) :-    % pass V
	s(_Type,X,_B,G-G,[i,give,the, couch,to,her],[]).
test(10,X) :-   % fail V
	s(_Type,X,_B,G-G,[i,gives,the, couch,to,her],[]).
test(11,X) :-   % fail V
	s(_Type,X,_B,G-G,[i,sleeps],[]).
test(12,X) :-   % pass V
	s(_Type,X,_B,G-G,[i,sleep],[]).
test(13,X) :-   % pass V
	s(_Type,X,_B,G-G,[she,sleeps],[]).
test(14,X) :-   % fail V
	s(_Type,X,_B,G-G,[she,sleep],[]).
test(15,X) :-   % fail V
	s(_Type,X,_B,G-G,[her,sleeps],[]).
test(16,X) :-   % pass V
	s(_Type,X,_B,G-G,[i,bet,you,a,couch,that,you,give,the,couch,to,me],[]).
test(17,X) :-   % fail V
	s(_Type,X,_B,G-G,[i,bet,you,a,couch,that,you,give,the,couch,to,i],[]).
test(18,X) :-   % pass V
	s(_Type,X,_B,G-G,[i,bet,you,a,couch,that,you,give,the,couch,to,him],[]).
test(19,X) :-   % pass V
	s(_Type,X,_B,G-G,[i,bet,you,a,couch,that,i,give,the,couch,to,him],[]).
test(20,X) :-   % pass V
	s(_Type,X,_B,G-G,[he,bets,you,a,couch,that,i,give,the,couch,to,him],[]).
test(21,X) :-   % fail V
	s(_Type,X,_B,G-G,[we,bets,you,a,couch,that,i,give,the,couch,to,him],[]).

test(22,X):-    % fail V
	s(_Type,X,_B,G-G,[sal,gave,a,picture,for,val],[]). % no
test(23,X):-    % pass V
	s(_Type,X,_B,G-G,[sal,gave,a,picture,to,val],[]). % yes
test(24,X):-    % pass V
	s(_Type,X,_B,G-G,[sal,looked,up,"sandy's",nose],[]).
test(25,X):-    % pass V
	s(_Type,X,_B,G-G,[sal,looked,up,"sandy's",number],[]).
test(26,X):-    % pass V
	s(_Type,X,_B,G-G,[it,was,val,that,looked,up,sal],[]).
test(27,X):-    % pass V
	s(_Type,X,_B,G-G,[it,was,val,that,sal,looked,up],[]).
test(28,X):-    % pass V
	s(_Type,X,_B,G-G,[it,was,val,who,gave,sal,a,picture,of,sal],[]).
test(29,X):-    % fail V
	s(_Type,X,_B,G-G,[it,was,up,"sandy's",number,that,sal,looked],[]). %no-yes, w/o sem
test(30,X):-    % pass V
	s(_Type,X,_B,G-G,[it,was,up,"sandy's",nose,that,sal,looked],[]).   %yes 
test(a30,X) :-  % pass X
	s(_Type,X,_B,G-G,[it,is,a,cat,that,hates,val],[]). %yes
test(b30,X) :-  % fail X
	s(_Type,X,_B,G-G,[it,is,a,cat,that,hate,val],[]). %no
test(c30,X) :-  % pass X
	s(_Type,X,_B,G-G,[it,is,cats,that,hate,val],[]). %yes
test(d30,X) :-  % fail V
	s(_Type,X,_B,G-G,[it,is,cats,that,hates,val],[]). %no

test(31,X):-    % pass V
	s(_Type,X,_B,G-G,[what,val,painted,was,a,picture,of,sal],[]).   %yes 
test(32,X):-    % fail V
	s(_Type,X,_B,G-G,[what,val,gave,was,a,picture,of,sal],[]).   %no
test(a32,X) :-  % pass X
	s(_Type,X,_B,G-G,[what,val,hates,are,cats],[]). %yes
test(b32,X) :-  % fail V
	s(_Type,X,_B,G-G,[what,val,hates,is,cats],[]). %?

test(33,X):-    % pass V
	s(_Type,X,_B,G-G,[what,val,gave,to,sal,was,a,picture,of,sal],[]). %yes (fails)
test(34,X):-    % pass V
	s(_Type,X,_B,G-G,[a,picture,of,sal,was,painted],[]).
test(35,X):-    % pass V
	s(_Type,X,_B,G-G,[a,picture,of,sal,was,painted,by,val],[]).
test(36,X):-    % pass V
	s(_Type,X,_B,G-G,[a,picture,of,sal,val,painted],[]).
test(37,X):-    % pass V
	s(_Type,X,_B,G-G,[what,val,gave,sal,was,a,picture,of,sal],[]).
test(38,X):-    % pass V
	s(_Type,X,_B,G-G,[what,val,gave,a,picture,was,a,charity],[]).
test(39,X):-    % pass V
	s(_Type,X,_B,G-G,[sal,believed,val,hated,a,cat],[]). %yes
test(40,X):-    % pass V
	s(_Type,X,_B,G-G,[sal,thought,val,hated,a,cat],[]).%yes
test(41,X):-    % pass V
	s(_Type,X,_B,G-G,[sal,suggested,val,hated,a,cat],[]).%yes
test(42,X):-    % fail V
	s(_Type,X,_B,G-G,[sal,asked,val,hated,a,cat],[]).%no
test(43,X):-    % fail V
	s(_Type,X,_B,G-G,[sal,wondered,val,hated,a,cat],[]).%no
test(44,X):-    % pass V
	s(_Type,X,_B,G-G,[sal,thought,val,a,cat],[]).%yes
test(45,X):-    % pass V
	s(_Type,X,_B,G-G,[sal,believed,val,a,cat],[]).%yes
test(46,X):-    % fail V
	s(_Type,X,_B,G-G,[sal,suggested,val,a,cat],[]). %no
test(47,X):-    % fail V
	s(_Type,X,_B,G-G,[sal,asked,val,a,cat],[]). %no
test(48,X):-    % fail V
	s(_Type,X,_B,G-G,[sal,wondered,val,a,cat],[]). %no
test(49,X):-    % fail V
	s(_Type,X,_B,G-G,[sal,thought,val],[]). %no
test(50,X):-    % pass V
	s(_Type,X,_B,G-G,[sal,believed,val],[]). %yes
test(51,X):-    % pass V
	s(_Type,X,_B,G-G,[sal,suggested,val],[]). %yes
test(52,X):-    % pass V
	s(_Type,X,_B,G-G,[sal,asked,val],[]). %yes
test(53,X):-    % fail V
	s(_Type,X,_B,G-G,[sal,wondered,val],[]). %no
test(54,X):-    % pass V
	s(_Type,X,_B,G-G,[sal,thought,val,to,hate,a,cat],[]).%yes
test(a54,X):-   % pass X
	s(_Type,X,_B,G-G,[a,cat,sal,thought,val,to,hate],[]).%yes
test(55,X):-    % pass V
	s(_Type,X,_B,G-G,[sal,believed,val,to,hate,a,cat],[]). %yes
test(a55,X):-   % pass X
	s(_Type,X,_B,G-G,[a,cat,sal,believed,val,to,hate],[]). %yes
test(56,X):-    % fail V
	s(_Type,X,_B,G-G,[sal,suggested,val,to,hate,a,cat],[]). %no
test(57,X):-    % pass V
	s(_Type,X,_B,G-G,[sal,asked,val,to,hate,a,cat],[]). %yes
test(58,X):-    % fail V
	s(_Type,X,_B,G-G,[sal,wondered,val,to,hate,a,cat],[]). %no
test(a58,X):-   % fail V
	s(_Type,X,_B,G-G,[a,cat,sal,wondered,val,to,hate],[]). %no

test(59,X):-    % fail V
	s(_Type,X,_B,G-G,[sal,thought,whether,val,believed,sal],[]).%no
test(60,X):-    % fail V
	s(_Type,X,_B,G-G,[sal,believed,whether,val,believed,sal],[]). %no
test(61,X):-    % pass V
	s(_Type,X,_B,G-G,[sal,suggested,whether,val,believed,sal],[]). %yes
test(62,X):-    % pass V
	s(_Type,X,_B,G-G,[sal,wondered,whether,val,believed,sal],[]). %yes
test(63,X):-    % pass V
	s(_Type,X,_B,G-G,[sal,asked,whether,val,believed,sal],[]). %yes

test(64,X):-    % fail V
	s(_Type,X,_B,G-G,[sal,thought,who,believed,sal],[]).%no
test(65,X):-    % fail V
	s(_Type,X,_B,G-G,[sal,believed,who,believed,sal],[]).%no
test(66,X):-    % fail V
	s(_Type,X,_B,G-G,[sal,thought,who,believed,sal],[]).%no
test(67,X):-    % pass V
	s(_Type,X,_B,G-G,[sal,suggested,who,believed,sal],[]).%yes
test(68,X):-    % pass V
	s(_Type,X,_B,G-G,[sal,wondered,who,believed,sal],[]).%yes
test(69,X):-    % pass V
	s(_Type,X,_B,G-G,[sal,asked,who,believed,sal],[]).%yes
test(70,X):-    % fail V
	s(_Type,X,_B,G-G,[sal,thought,who,sal,believed],[]).%no
test(71,X):-    % fail V
	s(_Type,X,_B,G-G,[sal,believed,who,sal,believed],[]).%no
test(72,X):-    % fail V
	s(_Type,X,_B,G-G,[sal,thought,who,sal,believed],[]).%no
test(73,X):-    % pass V
	s(_Type,X,_B,G-G,[sal,suggested,who,sal,believed],[]).%yes
test(74,X):-    % pass V
	s(_Type,X,_B,G-G,[sal,wondered,who,sal,believed],[]).%yes
test(75,X):-    % pass V
	s(_Type,X,_B,G-G,[sal,asked,who,sal,believed],[]).%yes
test(76,X):-    % pass V
	s(_Type,X,_B,G-G,[does,sal,believe,val],[]).%yes

test(77,X):-    % pass X
	s(_Type,X,_B,G-G,[it,is,sal,who,val,asks,lee,to,believe],[]).%yes
test(a77,X):-   % ???
	s(_Type,X,_B,G-G,[it,is,sal,that,val,asks,lee,to,believe],[]).%yes
test(78,X):-    % fail V
	s(_Type,X,_B,G-G,[it,is,sal,who,val,wonders,lee,to,believe],[]).%no
test(79,X):-    % pass X
	s(_Type,X,_B,G-G,[it,is,sal,who,val,thinks,lee,believes,hates,cats],[]).%yes
test(80,X):-    % fail V
	s(_Type,X,_B,G-G,[it,is,sal,who,val,thinks,lee,believes,hate,cats],[]).%no
test(81,X):-    % pass X
	s(_Type,X,_B,G-G,[it,is,dogs,who,val,thinks,lee,believes,hate,cats],[]).%yes
test(82,X):-    % fail V
	s(_Type,X,_B,G-G,[it,is,dogs,who,val,thinks,lee,believes,hates,cats],[]).%no

test(83,X):-    % pass X
	s(_Type,X,_B,G-G,[cats,val,thinks,lee,believes,hate,dogs],[]).%yes
test(a83,X):-   % fail V
	s(_Type,X,_B,G-G,[cats,val,thinks,lee,believes,that,hate,dogs],[]).%no
test(b83,X):-   % fail V
	s(_Type,X,_B,G-G,[cats,val,believes,that,hate,dogs],[]).%no
test(c83,X):-   % pass X
	s(_Type,X,_B,G-G,[cats,val,believes,hate,dogs],[]).%yes
test(84,X):-    % fail V
	s(_Type,X,_B,G-G,[cats,val,thinks,lee,believes,hates,dogs],[]).%no
test(85,X):-    % fail V
	s(_Type,X,_B,G-G,[cats,val,thinks,lee,wants,to,believe,hates,dogs],[]).%no
test(86,X):-    % pass X
	s(_Type,X,_B,G-G,[cats,val,thinks,lee,wants,to,believe,hate,dogs],[]).%yes

test(87,X) :-    % pass X
	s(_Type,X,_B,G-G,[it,seems,that,lee,hates,dogs],[]).
test(88,X) :-    % pass X
	s(_Type,X,_B,G-G,[it,seems,lee,hates,dogs],[]).
test(89,X) :-    % pass X
	s(_Type,X,_B,G-G,[lee,seems,to,hate,dogs],[]).
test(90,X) :-    % pass X
	s(_Type,X,_B,G-G,[lee,seems,to,want,to,hate,dogs],[]).
test(91,X) :-    % pass X
	s(_Type,X,_B,G-G,[it,is,lee,who,seems,to,want,to,hate,dogs],[]).
test(92,X) :-    % fail
	s(_Type,X,_B,G-G,[it,is,lee,who,seem,to,want,to,hate,dogs],[]). %no
test(93,X) :-    % fail V
	s(_Type,X,_B,G-G,[it,seems,whether,lee,hates,dogs],[]). %no
test(94,X) :-    % fail V
	s(_Type,X,_B,G-G,[it,seems,why,lee,hates,dogs],[]). %no
test(95,X) :-    % fail V
	s(_Type,X,_B,G-G,[it,seems,when,lee,hates,dogs],[]). %no
test(96,X) :-    % fail V
	s(_Type,X,_B,G-G,[it,seems,who,hates,dogs],[]). %no

test(97,X) :-    % pass X
	s(_Type,X,_B,G-G,[lee,is,the,person,that,val,gave,a,cat,to],[]). %yes
test(98,X) :-    % pass X
	s(_Type,X,_B,G-G,[lee,is,the,person,that,gave,a,cat,to,val],[]). %yes
test(99,X) :-    % pass X
	s(_Type,X,_B,G-G,[lee,is,the,person,that,gives,cats,to,a,charity],[]). %yes
test(100,X) :-   % pass X
	s(_Type,X,_B,G-G,[they,are,the,people,that,give,cats,to,a,charity],[]). %yes
test(101,X) :-   % fail V
	s(_Type,X,_B,G-G,[they,are,the,person,that,give,cats,to,a,charity],[]). %no
test(102,X) :-   % fail V
	s(_Type,X,_B,G-G,[they,are,the,people,that,gives,cats,to,a,charity],[]). %no
test(103,X) :-   % pass X
	s(_Type,X,_B,G-G,[they,are,the,people,that,val,thinks,give,cats,to,a,charity],[]). %yes
test(104,X) :-   % fail V
	s(_Type,X,_B,G-G,[they,are,the,people,that,val,thinks,that,give,cats,to,a,charity],[]).	%no
test(105,X) :-   % pass X
	s(_Type,X,_B,G-G,[it,is,the,answer,that,i,know,whether,lee,understands],[]).
test(106,X) :-   % fail V
	s(_Type,X,_B,G-G,[it,is,lee,that,i,know,whether,understands,the,answer],[]).
test(107,X) :-   % pass X
	s(_Type,X,_B,G-G,[it,is,she,who,they,think,sings],[]).
test(108,X) :-   % pass X
	s(_Type,X,_B,G-G,[it,is,they,who,she,thinks,sleep],[]).
test(109,X) :-   % fail V
	s(_Type,X,_B,G-G,[it,is,they,who,she,thinks,sleeps],[]).
test(110,X) :-   % fail V
	s(_Type,X,_B,G-G,[it,is,she,who,they,think,sing],[]).
test(111,X) :-   % pass V
	s(_Type,X,_B,G-G,[a,couch,i,bet,you,that,you,give,a,cat,to,me],[]).
test(112,X) :-   % pass X
	s(_Type,X,_B,G-G,[a,cat,i,bet,you,a,couch,that,you,give,to,me],[]).
test(113,X) :-   % pass X
	s(_Type,X,_B,G-G,[to,val,i,bet,you,a,couch,that,you,give,a,cat],[]).
test(114,X) :-   % pass X
	s(_Type,X,_B,G-G,[it,is,to,sal,that,i,bet,you,a,couch,that,you,give,a,cat],[]).
test(115,X) :-   % fail V
	s(_Type,X,_B,G-G,[it,is,you,that,i,bet,sal,a,couch,that,give,me,a,cat],[]).
test(116,X) :-   % pass X
	s(_Type,X,_B,G-G,[that,she,gives,me,a,cat,i,bet,sal,a,couch],[]).
test(117,X) :-   % pass X
	s(_Type,X,_B,G-G,[to,sal,val,gave,a,picture],[]).
test(118,X) :-   % pass X
	s(_Type,X,_B,G-G,[it,was,val,who,sal,gave,lee,a,picture,of],[]).
test(119,X) :-   % fail V
	s(_Type,X,_B,G-G,[val,it,is,a,cat,that,hates],[]).
test(120,X) :-   % fail V
	s(_Type,X,_B,G-G,[sal,what,val,painted,was,a,picture,of],[]).
test(121,X) :-   % pass X
	s(_Type,X,_B,G-G,[of,sal,what,val,painted,was,a,picture],[]).
test(a121,X) :-  % ???
	s(_Type,X,_B,G-G,[to,sal,what,val,gave,was,a,cat],[]).
test(122,X) :-   % fail V
	s(_Type,X,_B,G-G,[cats,what,val,hates,are],[]).
test(123,X) :-   % fail V
	s(_Type,X,_B,G-G,[cats,what,val,hates,is],[]).
test(124,X) :-   % pass X
	s(_Type,X,_B,G-G,[by,val,a,picture,of,sal,was,painted],[]).
test(125,X) :-   % pass X
	s(_Type,X,_B,G-G,[val,a,picture,of,sal,was,painted,by],[]).
test(126,X) :-   % pass X
	s(_Type,X,_B,G-G,[val,sal,believed,hates,a,cat],[]).
test(127,X) :-   % pass X
	s(_Type,X,_B,G-G,[val,sal,believed,hate,a,cat],[]).
test(a127,X) :-  % pass X
	s(_Type,X,_B,G-G,[you,sal,believes,hate,a,cat],[]).
test(b127,X) :-  % pass X
	s(_Type,X,_B,G-G,[i,sal,believes,hate,a,cat],[]).
test(c127,X) :-  % pass X
	s(_Type,X,_B,G-G,[they,sal,believes,hate,a,cat],[]).
test(128,X) :-   % pass X
	s(_Type,X,_B,G-G,[a,cat,sal,thought,val],[]).
test(129,X) :-   % pass X
	s(_Type,X,_B,G-G,[val,sal,believed,a,cat],[]).
test(130,X) :-   % fail V
	s(decl,X,_B,G-G,[a,cat,sal,suggested,val],[]).
test(131,X) :-   % pass X
	s(_Type,X,_B,G-G,[val,sal,thought,to,hate,dogs],[]).
test(132,X) :-   % fail V
	s(_Type,X,_B,G-G,[a,cat,sal,suggested,val,to,hate],[]).
test(133,X) :-   % fail V
	s(_Type,X,_B,G-G,[val,sal,suggested,whether,believed,sal],[]).
test(134,X) :-   % pass X
	s(_Type,X,_B,G-G,[lee,sal,wondered,whether,val,believed],[]).
test(135,X) :-   % pass X
	s(_Type,X,_B,G-G,[it,was,lee,sal,wondered,whether,val,believed],[]).
test(136,X) :-   % fail V
	s(_Type,X,_B,G-G,[val,sal,wondered,whether,believes,sal],[]).
test(a136,X) :-  % pass X
	s(_Type,X,_B,G-G,[whether,val,believes,sal,lee,wondered],[]).
test(b136,X) :-  % pass X
	s(_Type,X,_B,G-G,[sal,lee,wondered,whether,val,believes],[]).
test(137,X) :-   % pass V
	s(_Type,X,_B,G-G,[sal,thought,that,val,believes,sal],[]).
test(138,X) :-   % pass V
	s(_Type,X,_B,G-G,[sal,believed,that,val,believed,sal],[]).
test(139,X) :-   % pass V
	s(_Type,X,_B,G-G,[sal,suggested,that,val,believed,sal],[]).
test(140,X) :-   % fail V
	s(_Type,X,_B,G-G,[sal,wondered,that,val,believed,sal],[]).
test(141,X) :-   % fail V
	s(_Type,X,_B,G-G,[sal,asked,that,val,believed,sal],[]).
test(142,X) :-   % pass X
	s(_Type,X,_B,G-G,[sal,verified,that,val,believed,sal],[]).
test(143,X) :-   % pass X
	s(_Type,X,_B,G-G,[sal,said,that,val,believed,sal],[]).
test(144,X) :-   % pass X
	s(_Type,X,_B,G-G,[to,the,end,sal,said,that,val,believed,sal],[]).
test(a144,X) :-  % pass X
	s(_Type,X,_B,G-G,[sal,said,that,val,believed,sal,to,the,end],[]).
test(145,X) :-   % fail V
	s(_Type,X,_B,G-G,[it,is,lee,that,sal,said,that,believed,sal],[]).
test(146,X) :-   % pass X
	s(_Type,X,_B,G-G,[it,is,lee,that,sal,said,believed,sal],[]).
test(147,X) :-   % pass V
	s(_Type,X,_B,G-G,[who,believed,sal],[]).
test(148,X) :-   % pass X
	s(_Type,X,_B,G-G,[who,does,sal,believe],[]).
test(149,X) :-   % pass X
	s(_Type,X,_B,G-G,[who,does,sal,think,val,believes],[]).
test(150,X) :-   % pass X
	s(_Type,X,_B,G-G,[which,cat,does,sal,think,val,believes],[]).
test(151,X) :-   % fail V
	s(_Type,X,_B,G-G,[does,which,cat,sal,think,val,believes],[]).


test(666,X) :-   % pass V
	s(_Type,X,_B,G-G,[the,man,sleeps,on,the,couch],[]).

% initial fragment doesn't have information about
% scomp barriers, or gaps.
%s(Type,Parse,_BarrierInfo,_GapInfo,Start,Finish) :-
%	s(Type,Parse,Start,Finish).
	
%s(Type,Parse,_BarrierInfo,GapInfo,Start,Finish) :- s(Type,Parse,GapInfo,Start,Finish).  


%testem defined in gaputilities.pl
testem :-
     pass2([1,a1,b1,c1,d1,e1,2,5,7,9,12,13,16,18,19,20,23,24,25,26,27,28,30,a30,c30,31,a32,33,34,35,36,37,38,39,40,41,44,45,50,51,52,54,a54,55,a55,57,61,62,63,67,68,69,73,74,75,76,77,79,81,83,c83,86,87,88,89,90,91,97,98,99,100,103,105,107,108,111,112,113,114,116,117,118,121,124,125,126,127,a127,b127,c127,128,129,131,134,135,a136,b136,137,138,139,142,143,144,a144,146,147,148,149,150]),
     fail2([f1,3,4,6,8,10,11,14,15,17,21,22,b30,d30,32,b32,42,43,46,47,48,49,53,56,58,a58,59,60,64,65,66,70,71,72,78,80,82,a83,b83,84,85,92,93,94,95,96,101,102,104,106,109,110,115,119,120,122,123,130,132,133,136,140,141,145,151]).

testem2:-
	pass([a1,b1,c1,666]).
