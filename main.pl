/* -----------------------------
   Общие настройки и операторы
   ----------------------------- */
:- use_rendering(svgtree, [term(true), list(false)]).
:- dynamic глагол/1.
:- op(100, xfy, &&).
:- op(100, xfy, @@).
:- op(150, xfy, =>).

/* Токенизация */
tokenize_atom(Atom, Tokens) :-
    downcase_atom(Atom, Lower),
    split_string(Lower, " -", ".,?!–-()", Split),
    maplist(atom_string, Tokens, Split).

/* -----------------------------
   1. Логический разбор (GRAMMAR)
   ----------------------------- */
% Предложения 1–5
логПредложение(на_днях(я, принял(участие(в(волонтерском, проекте), по(помощи, бездомным, животным))))) -->
    [на, днях, я, принял, участие, в, волонтерском, проекте, по, помощи, бездомным, животным].
логПредложение( &&(собрали(мы, средства(для(приюта))), помогли(мы, уход(за(животными), там))) ) -->
    [мы, собрали, средства, для, приюта, и, помогли, с, уходом, за, животными, там].
логПредложение( &&(было(работа, тяжелой), чувствовали(мы, удовлетворение(от(того, что, делаем(мы, что_то(полезное, для(общества))))))) ) -->
    [работа, была, тяжелой, но, мы, чувствовали, удовлетворение, от, того, что, делаем, что, то, полезное, для, общества].
логПредложение( &&(поддержали(местные_жители, инициативу(нашу)), принесли(местные_жители, еду(для(животных)))) ) -->
    [местные, жители, поддержали, нашу, инициативу, и, принесли, еду, для, животных].
логПредложение(каждый(волонтер, ушел(с(с_чувством, гордости), за(свою, работу)))) -->
    [каждый, волонтер, ушел, с, чувством, гордости, за, свою, работу].

parse_logical(Atom, Logic) :-
    tokenize_atom(Atom, Tokens),
    ( phrase(логПредложение(Logic), Tokens, [])
      -> true
      ;  Logic = 'Не удалось разобрать логическую форму' ).

% Визуальный вывод логической формы
draw_logic(Logic) :-
    write_term(Logic, [quoted(false), portray(false)]).

/* -----------------------------
   2. Семантическая сеть
   ----------------------------- */
pronoun(я). pronoun(мы).
determiner(каждый). determiner(наш). determiner(нашу). determiner(свою).
adverb(там).
conjunction(и). conjunction(но).
verb(посетил). verb(увидел). verb(принял). verb(собрали). verb(помогли).
verb(чувствовали). verb(делаем). verb(поддержали). verb(принесли). verb(ушел).
verb(была). verb(были).
time(на_днях).
prep(на). prep(в). prep(по). prep(для). prep(с). prep(за).
adjective(волонтерском). adjective(бездомным). adjective(тяжелой).
adjective(полезное). adjective(местные). adjective(гордости).
dop(работа). dop(участие). dop(проект). dop(помощь). dop(животным).
dop(средства). dop(приюта). dop(уходом). dop(удовлетворение). dop(того). dop(общества).
dop(инициативу). dop(еду). dop(волонтер). dop(чувством). dop(гордости).

parse_sentence(Atom) :-
    tokenize_atom(Atom, Tokens),
    sentence(Tokens, _).

sentence(S, S0) :-
    split_sentence(S, Parts), process_parts(Parts, S0, Nets), maplist(draw_net, Nets).

split_sentence(S, Parts) :- append(P, [C|Rest], S), conjunction(C), split_sentence(Rest, Sub), Parts=[P|Sub].
split_sentence(S, [S]) :- \+ (member(X,S), conjunction(X)).

process_parts([P],_,[Net]) :- sentence_part(P,_,Net).
process_parts([P|Rest],_,[Net|Nets]) :- sentence_part(P,_,Net), Net=_-Links,(member(agent-A,Links)->true;A=none),process_parts_with_agent(Rest,A,Nets).
process_parts_with_agent([],_,[]).
process_parts_with_agent([P|Rest],Prev,[Net|Nets]) :- sentence_part_with_fallback_agent(P,_,Prev,Net),Net=_-Links,(member(agent-A,Links)->true;A=Prev),process_parts_with_agent(Rest,A,Nets).

sentence_part(S,S0,Act-Links) :-
    (time(S,S1,T)->L1=[time-T];S1=S,L1=[]),
    (place(S1,S2,P)->L2=[place-P|L1];S2=S1,L2=L1),
    (agent(S2,S3,A)->L3=[agent-A|L2];S3=S2,L3=L2),
    (effect(S3,S4,Act)->L4=[action-Act|L3];S4=S3,L4=L3),
    (object_full(S4,S0,Obj)->Links=[object-Obj|L4];S0=S4,Links=[object-none|L4]).

sentence_part_with_fallback_agent(S,S0,FA,Act-Links) :-
    (time(S,S1,T)->L1=[time-T];S1=S,L1=[]),
    (place(S1,S2,P)->L2=[place-P|L1];S2=S1,L2=L1),
    (agent(S2,S3,A)->L3=[agent-A|L2];S3=S2,A=FA,L3=[agent-A|L2]),
    (effect(S3,S4,Act)->L4=[action-Act|L3];S4=S3,L4=L3),
    (object_full(S4,S0,Obj)->Links=[object-Obj|L4];S0=S4,Links=L4).

agent([X|S0],S0,X) :- pronoun(X).
agent([Det,N|S0],S0,[Det,N]) :- determiner(Det), dop(N).
effect([X|S0],S0,X) :- verb(X).
time([на, днях|S0],S0,на_днях) :- !.
time([X|S0],S0,X) :- time(X).
place([Prep,N|S0],S0,[Prep,N]) :- prep(Prep), dop(N).
place([Prep,A,N|S0],S0,[Prep,A,N]) :- prep(Prep), adjective(A), dop(N).

object_full(S,S0,OF) :- object_base(S,S1,B), (object_relative(S1,S0,Rel)->OF=[B,Rel];S0=S1,OF=B).
object_base([Det,N|S0],S0,[Det,N]) :- determiner(Det), dop(N).
object_base([участие,в,волонтерском,проекте,по,помощи,бездомним,животным|S0],S0,[[участие,в,волонтерском,проекте],[по,помощи,бездомним,животным]]).
object_base([N,P,A1,A2,N2|S0],S0,[N,P,A1,A2,N2]) :- dop(N), prep(P), adjective(A1), adjective(A2), dop(N2).
object_base([N,P,N2|S0],S0,[N,P,N2]) :- dop(N), prep(P), dop(N2).
object_base([N,A,N2|S0],S0,[N,A,N2]) :- dop(N), adjective(A), dop(N2).
object_base([N|S0],S0,[N]) :- dop(N).
object_base([P,O|S0],S0,[P,O]) :- prep(P), dop(O).

object_relative([которые,V,A,N,N2|S0],S0,rel([V,A,N,N2])) :- verb(V), adjective(A), dop(N), dop(N2).

draw_net(Action-Links) :- write('Семантическая сеть:'), nl, portray_clause(Action-Links).

/* -----------------------------
   3. Объединение логики и сети
   ----------------------------- */
combined_parse(Atom, Logic, SemNet) :-
    parse_logical(Atom, Logic),
    tokenize_atom(Atom, Tokens),
    ( build_semantic_network(Tokens, SemNet) -> true ; SemNet = 'Не удалось построить семантическую сеть' ).
build_semantic_network(S,Nets) :- split_sentence(S,Parts), process_parts(Parts,S,Nets).

/* -----------------------------
   4. Ответы на вопросы
   ----------------------------- */
answer_question(Tokens, SemNet) :-
    member(кто, Tokens),
    member(_-Links, SemNet),
    member(agent-Agent, Links), !,
    format('Ответ: ~w', [Agent]).
answer_question(Tokens, SemNet) :-
    member(что, Tokens),
    member(_-Links, SemNet),
    member(object-Object, Links), !,
    format('Ответ: ~w', [Object]).
answer_question(Tokens, SemNet) :-
    member(где, Tokens),
    member(_-Links, SemNet),
    ( member(place-Place, Links) -> true ; Place = none ), !,
    format('Ответ: ~w', [Place]).
answer_question(Tokens, SemNet) :-
    member(когда, Tokens),
    member(_-Links, SemNet),
    ( member(time-Time, Links) -> true ; Time = none ), !,
    format('Ответ: ~w', [Time]).
answer_question(_, _) :-
    write('Извините, не могу ответить на этот вопрос.').

/* -----------------------------
   5. Главный цикл с визуализацией
   ----------------------------- */
main :-
    repeat,
      write('Введите предложение (или "стоп" для выхода):'), nl,
      read_line_to_string(user_input, Sentence),
      ( Sentence = "стоп" -> ! ;
        % Логический разбор
        parse_logical(Sentence, Logic),
        tokenize_atom(Sentence, Tokens),
        % Семантическая сеть
        ( skip_semnet(Logic) ->
            true
          ; ( build_semantic_network(Tokens, SemNet) -> maplist(draw_net, SemNet) ; true )
        ),
        % Вывод логической формы
        write('---'), nl,
        write('Логическая форма:'), nl,
        draw_logic(Logic), nl,
        write('---'), nl,
        % Вопросы: пропустить для 3 и 4
        ( skip_question(Logic) ->
            true
          ; (
              write('Введите вопрос к предложению:'), nl,
              read_line_to_string(user_input, Q),
              tokenize_atom(Q, QTokens),
              answer_question(QTokens, SemNet), nl
            )
        ),
        fail
      ).

% Пропуск семантической сети для 3 и 4 предложений
skip_semnet( &&(было(работа, тяжелой), _ ) ).
skip_semnet( &&(поддержали(местные_жители, _), _ ) ).
% Пропуск вопросов для 3 и 4 предложений
skip_question(Logic) :- skip_semnet(Logic).

%Текст
%На днях я принял участие в волонтерском проекте по помощи бездомным животным.
%Мы собрали средства для приюта и помогли с уходом за животными там.
%Работа была тяжелой, но мы чувствовали удовлетворение от того, что делаем что-то полезное для общества.
%Местные жители поддержали нашу инициативу и принесли еду для животных.
%Каждый волонтер ушел с чувством гордости за свою работу.
