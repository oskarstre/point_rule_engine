:- module(multi, [
             find_max_points/3, best_of/3
          ]).

:- use_module(library(apply)).


get_points((Type, Rate, Bonus, RuleId), Amount, (Type, Rate, Bonus, RuleId, Points)) :-
    Points is Bonus + (Rate * Amount), !.

max_points((_, _, _, _, Points1),  (Type2, Rate2, Bonus2, RuleId2, Points2),  R) :-
    Points2 >= Points1,
    R =  (Type2, Rate2, Bonus2, RuleId2, Points2), !.
max_points(A,_, A).

find_max_points(Points, Purchase, Max) :-
   purchase(_, _, Price, _, _, _, _) = Purchase,
   maplist({Price}/[In,Out]>>get_points(In, Price, Out), Points, WithPoints),
   foldl(max_points, WithPoints, [], Max).

best_of(Points, Purchase, NewPoints) :-
   find_max_points(Points, Purchase, (_,_,_,Best, _)),
   include({Best}/[In]>> (In = (_,_,_,Best)), Points, NewPoints).

best_of(_, Points, [], Points).
best_of(Purchase, Points, Excludes, NewPoints) :-
   best_of(Points, Purchase, [(_,_,_,BestId)]),
   maplist([In,Id]>> (In = (_,_,_,Id)), Points, Ids),
   subtract(Ids, Excludes, T1),
   append(T1, [BestId], NewIds),
   include({NewIds}/[In]>>(In = (_,_,_,Id), memberchk(Id,NewIds)), Points, NewPoints).


:- begin_tests(multi).
:- use_module(levels).
:- use_module(declarations).
:- use_module(helpers).
:- use_module(rules).
:- use_module(defaults).
:- use_module(multi).

mockup :-
    reset_data,
    retractall(price_convert_rate(_,_,_,_,_,_,_,_,_)),
    new_point_type(default),
    add_price_convert_rate(default, web, *, *, *, *,10, 100, basic_rule),
    add_price_convert_rate(default, web, *, *, *, *,30, 300, extra_rule),
    add_price_convert_rate(default, web, *, *, *, *,20, 200, extra_rule2).


test(many_rules) :-
    mockup,
    P1 = purchase(petter, product1, 100, web, norway, *, date(2019,1,1)),
    get_all_points_from_purchase(P1, Points),
    Points == [(default,20,200,extra_rule2),(default,30,300,extra_rule),(default,10,100,basic_rule)].

test(find_best_of) :-
    mockup,
    P1 = purchase(petter, product1, 100, web, norway, *, date(2019,1,1)),
    get_all_points_from_purchase(P1, Points),
    best_of(P1, Points, [extra_rule, extra_rule2], NewPoints),
    writeln(NewPoints).











