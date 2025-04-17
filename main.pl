/* -----------------------------
   Общие настройки и операторы
   ----------------------------- */
:- use_rendering(svgtree, [list(false)]).
:- dynamic глагол/1.
:- op(100, xfy, &&).
:- op(100, xfy, @@).
:- op(150, xfy, =>).

/* Токенизация: преобразует строку в список токенов */
tokenize_atom(Atom, Tokens) :-
    downcase_atom(Atom, Lower),
    split_string(Lower, " ", ".,?!–-()", Split),
    maplist(atom_string, Tokens, Split).

/* -----------------------------
   1. Логический разбор предложений
   ----------------------------- */

% Предложение 1
логПредложение(на_днях(я, принял(Obj))) -->
    time_phrase(на_днях), subject(я), verb3(принял), object_part(Obj).

% Предложение 2
логПредложение( &&(собрали(мы, средства(для(приюта))), помогли(мы, уход(за(животными), там))) ) -->
    [мы, собрали, средства, для, приюта, и, помогли, с, уходом, за, животными, там].

% Предложение 3
логПредложение( &&(было(работа, тяжелой), чувствовали(мы, удовлетворение(от(того, что, делаем(мы, что_то(полезное, для(общества))))))) ) -->
    [работа, была, тяжелой, но, мы, чувствовали, удовлетворение, от, того, что, делаем, что-то, полезное, для, общества].

% Предложение 4
логПредложение( &&(поддержали(местные_жители, инициативу(нашу)), принесли(местные_жители, еду(для(животных)))) ) -->
    [местные, жители, поддержали, нашу, инициативу, и, принесли, еду, для, животных].

% Предложение 5
логПредложение(каждый(волонтер, ушел(с(с_чувством, гордости), за(свою, работу)))) -->
    [каждый, волонтер, ушел, с, чувством, гордости, за, свою, работу].


% Нетерминалы для логического разбора
subject(я) --> [я].
verb1(посетил) --> [посетил].
verb2(увидел)  --> [увидел].
verb3(принял)  --> [принял].
% новые глаголы
verb4(собрали) --> [собрали].
verb5(помогли) --> [помогли].
verb6(чувствовали) --> [чувствовали].
verb7(поддержали) --> [поддержали].
verb8(принесли) --> [принесли].
time_phrase(на_днях) --> [на, днях].

object_part(Obj) -->
    [участие], prepositional_object(Obj).

prepositional_object(Obj) -->
    [в], [волонтерском], [проекте],
    [по], [помощи], [бездомным], [животным],
    { Obj = участие(в(волонтерском, проекте), по(помощи, бездомным, животным)) }.

parse_logical(Atom, Logic) :-
    tokenize_atom(Atom, Tokens),
    ( phrase(логПредложение(Logic), Tokens, [])
      -> true
      ;  Logic = 'Не удалось разобрать логическую форму' ).

/* -----------------------------
   2. Построение семантической сети
   ----------------------------- */

% Словарь
pronoun(я).
determiner(каждый). determiner(своими). determiner(новыми). determiner(наших).
adverb(там). conjunction(и). conjunction(но).

verb(посетил). verb(были). verb(представлены). verb(увидел).
verb(упрощают). verb(делились). verb(ушел). verb(принял).
% новые для семантического
verb(собрали). verb(помогли). verb(чувствовали). verb(поддержали). verb(принесли). verb(делаем).

time(недавно). time(на_днях).
time_phrase(на, днях).

prep(в). prep(на). prep(для). prep(с). prep(о). prep(по).

adjective(новых). adjective(инновационные). adjective(различных).
adjective(повседневную). adjective(умного). adjective(нашем).
adjective(волонтерском). adjective(бездомным).
adjective(тяжелой). adjective(полезное). adjective(местные).

% Существительные-объекты
dop(выставка). dop(выставку). dop(технологии). dop(технологий).
dop(город). dop(городе). dop(гаджеты). dop(решения). dop(дом).
dop(демонстрации). dop(устройства). dop(устройств).
dop(жизнь). dop(люди). dop(людей). dop(эксперты). dop(знания).
dop(будущее). dop(влияние). dop(общество). dop(посетитель).
dop(идеи). dop(вдохновение).
dop(участие). dop(проект). dop(помощь). dop(животные). dop(животным).
% для новых предложений
dop(средства). dop(приюта). dop(уходом). dop(инициатива). dop(работу). dop(волонтер). dop(чувством). dop(гордости). dop(еду).

environment(там).

% Построение сети
parse_sentence(Atom) :- tokenize_atom(Atom, Tokens), sentence(Tokens, _).

sentence(S, S0) :- split_sentence(S, Parts), process_parts(Parts, S0, Nets), maplist(draw_net, Nets).
split_sentence(S, Parts) :- append(Part, [C|Rest], S), conjunction(C), split_sentence(Rest, Sub), Parts=[Part|Sub].
split_sentence(S, [S]) :- \+ (member(X,S), conjunction(X)).

process_parts([Part], _, [Net]) :- sentence_part(Part, _, Net).
process_parts([Part|Rest], _, [Net|Nets]) :-
    sentence_part(Part, _, Net), Net=_-Links,
    ( member(agent-Agent,Links) -> true ; Agent=none ),
    process_parts_with_agent(Rest, Agent, Nets).

process_parts_with_agent([],_,[]).
process_parts_with_agent([Part|Rest],PrevA,[Net|Nets]) :-
    sentence_part_with_fallback_agent(Part,_,PrevA,Net), Net=_-Links,
    ( member(agent-A,Links)->true ; A=PrevA ),
    process_parts_with_agent(Rest,A,Nets).

% Семантический разбор
sentence_part(S,S0,Action-Links) :-
    ( time(S,S1,T)->Links1=[time-T];S1=S,Links1=[]),
    ( place(S1,S2,Pl)->Links2=[place-Pl|Links1];S2=S1,Links2=Links1),
    ( agent(S2,S3,A)->Links3=[agent-A|Links2];S3=S2,Links3=Links2),
    ( effect(S3,S4,Action)->Links4=[action-Action|Links3];S4=S3,Links4=Links3),
    ( object_full(S4,S0,Obj)->Links=[object-Obj|Links4];S0=S4,Links=[object-none|Links4]).

sentence_part_with_fallback_agent(S,S0,FA,Action-Links) :-
    ( time(S,S1,T)->L1=[time-T];S1=S,L1=[]),
    ( place(S1,S2,Pl)->L2=[place-Pl|L1];S2=S1,L2=L1),
    ( agent(S2,S3,A)->L3=[agent-A|L2];S3=S2,A=FA,L3=[agent-A|L2]),
    ( effect(S3,S4,Action)->L4=[action-Action|L3];S4=S3,L4=L3),
    ( object_full(S4,S0,Obj)->Links=[object-Obj|L4];S0=S4,Links=L4).

agent([X|S0],S0,X) :- pronoun(X).
agent([Det,N|S0],S0,[Det,N]) :- determiner(Det), dop(N).
effect([X|S0],S0,X) :- verb(X).
time([на, днях|S0],S0,на_днях) :- !.
time([X|S0],S0,X) :- time(X).
place([Prep,Adj,N|S0],S0,[Prep,Adj,N]) :- prep(Prep), adjective(Adj), dop(N).
place([Prep,N|S0],S0,[Prep,N]) :- prep(Prep), dop(N).

object_full(S,S0,OF) :- object_base(S,S1,B), (object_relative(S1,S0,Rel)->OF=[B,Rel];S0=S1,OF=B).
object_base([участие,в,волонтерском,проекте,по,помощи,бездомным,животным|S0],S0,[[участие,в,волонтерском,проекте],[по,помощи,бездомным,животным]]).
object_base([N,P,A1,A2,N2|S0],S0,[N,P,A1,A2,N2]) :- dop(N),prep(P),adjective(A1),adjective(A2),dop(N2).
object_base([N,P,A,N2|S0],S0,[N,P,A,N2]) :- dop(N),prep(P),adjective(A),dop(N2).
object_base([N,P,N2|S0],S0,[N,P,N2]) :- dop(N),prep(P),dop(N2).
object_base([N,A,N2|S0],S0,[N,A,N2]) :- dop(N),adjective(A),dop(N2).
object_base([A1,A2,N|S0],S0,[A1,A2,N]) :- adjective(A1),adjective(A2),dop(N).
object_base([A,N|S0],S0,[A,N]) :- adjective(A),dop(N).
object_base([N|S0],S0,[N]) :- dop(N).
object_base([N1,N2|S0],S0,[N1,N2]) :- dop(N1),dop(N2).
object_base([P,O|S0],S0,[P,O]) :- prep(P),dop(O).
object_base([P,A,O|S0],S0,[P,A,O]) :- prep(P),adjective(A),dop(O).

object_relative([которые,V,A,N,N2|S0],S0,rel([V,A,N,N2])) :- verb(V),adjective(A),dop(N),dop(N2).

draw_net(Act-Links) :- write('Семантическая сеть:'), nl, portray_clause(Act-Links).

/* -----------------------------
   3. Объединение логики и сети
   ----------------------------- */
combined_parse(Atom, Logic, SemNet) :-
    tokenize_atom(Atom, Tokens),
    ( phrase(логПредложение(Logic), Tokens, []) -> true ; Logic = 'Не удалось разобрать логическую форму' ),
    ( build_semantic_network(Tokens, SemNet) -> true ; SemNet = 'Не удалось построить семантическую сеть' ).
build_semantic_network(S, Nets) :- split_sentence(S, Parts), process_parts(Parts, S, Nets).

/* -----------------------------
   4. Главный цикл с ручным вводом
   ----------------------------- */
main :-
    repeat,
      write('Введите предложение (или "стоп" для выхода):'), nl,
      read_line_to_string(user_input, Sentence),
      ( Sentence = "стоп" -> ! ;
        combined_parse(Sentence, Logic, SemNets),
        write('---'), nl,
        write('Логическая форма:'), nl,
        portray_clause(Logic),
        write('---'), nl,
        maplist(draw_net, SemNets),
        write('===\n'),
        fail
      ).
