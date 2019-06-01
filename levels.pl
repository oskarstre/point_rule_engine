:- module(levels, [
              update_person_level/1
                ]).

:- use_module(declarations).
:- use_module(helpers).

add(X,Y,Sum):- Sum is X+Y.

update_person_level(CustomerId) :-
    person_level(CustomerId, PersonLevel),
    setof(PointsType,  point(CustomerId, PointsType, Points, _, _,_), PointTypes),
    member(PT, PointTypes),
    findall(Points, point(CustomerId, PT, Points, _Purchase, _,_), AllPoints),
    foldl(add, AllPoints, 0, Total),
    level(Level, PT, LevelPoints, _Name),
    Level > PersonLevel,
    Total >= LevelPoints,
    retractall(person_level(CustomerId, PersonLevel)),
    assert(person_level(CustomerId, Level)).

update_person_level(_) :- !.

:- begin_tests(levels).
:- use_module(levels).
:- use_module(declarations).
:- use_module(helpers).
:- use_module(rules).
:- use_module(defaults).

mockup :-
    reset_data,
    new_level(0, default, 0, silver),
    new_level(1, default, 100, gold),
    assert(person_level(petter, 0)),
    retractall(price_convert_rate(_,_,_,_,_,_,_,_,_)),
    new_point_type(default),
    add_price_convert_rate(default, web, *, *, *, *,10, 100, basic_rule).


% after two purchases, the customer should qualify for a level upgrade
% to gold
test(person_inc_level) :-
    mockup,
    % use new_purchase in prod!
    P1 = purchase(petter, product1, 100, web, norway, *, date(2019,1,1)),
    assert(P1),
    get_all_points_from_purchase(P1, Points1),
    add_points(Points1, P1, date(2030,1,1)),
    P2 = purchase(petter, product2, 100, web, norway, *, date(2019,1,2)),
    assert(P2),
    get_all_points_from_purchase(P2, Points2),
    add_points(Points2, P2, date(2030,1,1)),
    findall(point(A,B,C,D,E,F), point(A,B,C,D,E,F), All),
    All = [point(petter, default, 1100, purchase(petter, product1, 100, web, norway, *, date(2019, 1, 1)), date(2030, 1, 1), basic_rule), point(petter, default, 1100, purchase(petter, product2, 100, web, norway, *, date(2019, 1, 2)), date(2030, 1, 1), basic_rule)],
    update_person_level(petter),
    person_level(petter,1), !.

% no points from rule if it needs level 1
test(level_rule_constraint_no_points) :-
    mockup,
    price_convert_rate(_,_,_,_,_,_,_,_,basic_rule),
    P1 = purchase(petter, product1, 100, web, norway, *, date(2019,1,1)),
    new_level_constraint(basic_rule, 1),
    get_all_points_from_purchase(P1, []).

% but points from rule if rule needs only level 0
test(level_rule_constraint_got_points) :-
    mockup,
    price_convert_rate(_,_,_,_,_,_,_,_,basic_rule),
    P1 = purchase(petter, product1, 100, web, norway, *, date(2019,1,1)),
    new_level_constraint(basic_rule, 0),
    get_all_points_from_purchase(P1, [(default,10,100,basic_rule)]).


