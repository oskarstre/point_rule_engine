:- module(multi, [
             find_max_points/3, best_of/3, max_points/3
          ]).

:- use_module(library(apply)).


get_points(point(Type, Rate, Bonus, RuleId), Amount, point(Type, Rate, Bonus, RuleId, Points)) :-
    Points is Bonus + (Rate * Amount), !.

max_points(point(_, _, _, _, Points1),  point(Type2, Rate2, Bonus2, RuleId2, Points2),  R) :-
    Points2 >= Points1,
    R =  point(Type2, Rate2, Bonus2, RuleId2, Points2), !.
max_points(A,_, A).

find_max_points(Points, Purchase, Max) :-
   purchase(_, _, Price, _, _, _, _) = Purchase,
   maplist({Price}/[In,Out]>>get_points(In, Price, Out), Points, WithPoints),
   foldl(max_points, WithPoints, [], Max).


best_of(Points, Purchase, NewPoints) :-
   find_max_points(Points, Purchase, point(_,_,_,Best, _)),
   include({Best}/[In]>> (In = point(_,_,_,Best)), Points, NewPoints).

best_of(_, _, [], _).
best_of(Purchase, Points, Excludes, NewPoints) :-
   best_of(Points, Purchase, [point(_,_,_,BestId)]),
   include({Excludes, BestId}/[In]>>(In = point(_,_,_,Id), (not(memberchk(Id, Excludes)) ; Id == BestId), !), Points, NewPoints).


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

% one purchase could match many rules
test(many_rules) :-
    mockup,
    P1 = purchase(petter, product1, 100, web, norway, *, date(2019,1,1)),
    get_all_points_from_purchase(P1, Points),
    Points == [point(default,20,200,extra_rule2),point(default,30,300,extra_rule),point(default,10,100,basic_rule)].

% which of the rules are best
test(find_best_of) :-
    mockup,
    P1 = purchase(petter, product1, 100, web, norway, *, date(2019,1,1)),
    get_all_points_from_purchase(P1, Points),
    best_of(P1, Points, [extra_rule, extra_rule2, extra_rule3], NewPoints),
    NewPoints == [point(default,30,300,extra_rule),point(default,10,100,basic_rule)].

% allow only one extra_rule
test(not_together) :-
    mockup,
    P1 = purchase(petter, product1, 100, web, norway, *, date(2019,1,1)),
    new_not_together([extra_rule, extra_rule2]),
    Points = [point(default,40,400,extra_rule2), point(default,30,300,extra_rule), point(default,10,100,basic_rule)],
    handle_constraint_not_together(P1, Points, NewPoints),
    NewPoints == [point(default,40,400,extra_rule2),point(default,10,100,basic_rule)].


% allow only one if all is not togheter
test(not_together_one) :-
    mockup,
    P1 = purchase(petter, product1, 100, web, norway, *, date(2019,1,1)),
    new_not_together([basic_rule, extra_rule, extra_rule2]),
    Points = [point(default,40,400,extra_rule2), point(default,30,300,extra_rule), point(default,10,100,basic_rule)],
    handle_constraint_not_together(P1, Points, NewPoints),
    NewPoints == [point(default,40,400,extra_rule2)].

test(not_together_two_rules) :-
    mockup,
    P1 = purchase(petter, product1, 100, web, norway, *, date(2019,1,1)),
    new_not_together([extra_rule, extra_rule2]),
    new_not_together([extra_rule2, basic_rule]),
    Points = [point(default,40,400,extra_rule2), point(default,30,300,extra_rule), point(default,10,100,basic_rule)],
    handle_constraint_not_together(P1, Points, NewPoints),
    NewPoints == [point(default,40,400,extra_rule2)].





