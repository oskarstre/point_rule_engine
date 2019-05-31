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
    findall(Points, point(CustomerId, PT, Points, _Purchase, _,_), Points),
    foldl(add, Points, 0, Total),
    level(Level, PT, LevelPoints, _Name),
    Level > PersonLevel,
    Total >= LevelPoints,
    retractall(person_level(CustomerId, PersonLevel)),
    assert(person_level(CustomerId, Level)).



:- begin_tests(levels).
:- use_module(levels).
:- use_module(declarations).
:- use_module(helpers).
:- use_module(rules).
:- use_module(defaults).

mockup :-
    reset_data,
    assert(person_level(petter, gold)),
    retractall(price_convert_rate(_,_,_,_,_,_,_,_,_)),
    new_point_type(default),
    add_price_convert_rate(default, web, *, *, *, *,10, 100, basic_rule),
    % use new_purchase in prod!
    P1 = purchase(petter, product1, 100, web, norway, *, date(2019,1,1)),
    assert(P1),
    get_all_points_from_purchase(P1, Points1),
    add_points(Points1, P1, date(2030,1,1)),
    P2 = purchase(petter, product2, 100, web, norway, *, date(2019,1,2)),
    assert(P2),
    get_all_points_from_purchase(P2, Points2),
    add_points(Points2, P2, date(2030,1,1)),
    new_level(0, default, 0, silver),
    new_level(1, default, 100, gold).

test(person_inc_level) :-
    mockup.

