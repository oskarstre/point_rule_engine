:- module(rules, [
              get_all_points_from_purchase/2, add_points/3, handle_constraint_not_together/3, handle_constraint_not_before/2,
              add_point_constraint_total_time/4
                ]).

:- use_module(declarations).
:- use_module(helpers).
:- use_module(library(apply)).
:- use_module(multi).


handle_constraint_total_time(RuleId, _, _) :- not(point_constraint_total_time(RuleId, _, _, _)), !.
handle_constraint_total_time(RuleId, PersonId, Date) :-
    point_constraint_total_time(RuleId, NeededAmount, WithinDays, NeededNumberOfPurchases),
    purchases_within_days(PersonId, WithinDays, Date, (Purchases, Value)),
    (   (NeededAmount == *; Value >= NeededAmount) -> true ; fail ),
    length(Purchases, NumPurchases),
    (   (NeededNumberOfPurchases == * ; NumPurchases >= NeededNumberOfPurchases) -> true ; fail), !.


handle_constraint_level(RuleId, _) :-  not(level_constraint(RuleId, _)), !.
handle_constraint_level(RuleId, CustomerId) :-
    person_level(CustomerId, Level),
    level_constraint(RuleId, MinLevel),
    Level >= MinLevel, !.

not_together_rec(_, P, [], P) :- !.
not_together_rec(Purchase, Points, [H|T], NewPoints) :-
    not_together(H, Excludes),
    include({Excludes}/[P]>>(P = point(_,_,_,RuleId), memberchk(RuleId, Excludes)), Points, PointsExcludes),
    best_of(Purchase, PointsExcludes, Excludes, [point(_,_,_,BestId)]),
    exclude({Excludes, BestId}/[P]>>(P = point(_,_,_,RuleId), RuleId \= BestId, memberchk(RuleId, Excludes)), Points, Points2),
    not_together_rec(Purchase, Points2, T, NewPoints).

handle_constraint_not_together(Purchase, Points, NewPoints) :-
    maplist([In,RuleId]>>(In = point(_, _, _, RuleId)), Points, PointIds),
    findall(P, (member(P, PointIds), not_together(P,_)), ExcludeCandidates),
    not_together_rec(Purchase, Points, ExcludeCandidates, NewPoints), !.

handle_constraint_not_before(RuleId, CustomerId) :-
    point(CustomerId, _, _, _, _, OtherRuleId),
    not_before(OtherRuleId, RuleId),
    fail, !.

handle_constraint_not_before(_, _) :- !.

point_logic(Purchase, price_convert_rate(_PointType, CChannel, CLocation, CCampaign, CProduct, CCategory,_ConvRate, _AddPoints, RuleId)) :-
    purchase(PersonId, Product, _Price, Channel, Location, Campaign, Date) = Purchase,
    (   (CCategory == * ; in_category2(Product, CCategory)) -> true ; fail ),
    (   (CProduct == Product ; CProduct == *) -> true ; fail ),
    (   (CChannel == Channel ; CChannel == *) -> true ; fail ),
    (   (CLocation == Location ; CLocation == *) -> true ; fail),
    (   (CCampaign == Campaign ; CCampaign == *) -> true ; fail),
    handle_constraint_total_time(RuleId, PersonId, Date),
    handle_constraint_level(RuleId, PersonId),
    handle_constraint_not_before(RuleId, PersonId).


points_date(purchase(PersonId, ProductId, Price, Channel, Location, Campaign, Date), point(PointType, ConvRate, AddPoints, RuleId)) :-
    price_convert_rate(PointType, CChannel, CLocation, CCampaign, CProduct, CCategory, ConvRate, AddPoints, StartDate, EndDate, RuleId),
    point_logic(purchase(PersonId, ProductId, Price, Channel, Location, Campaign, Date),
                price_convert_rate(PointType, CChannel, CLocation, CCampaign, CProduct, CCategory, ConvRate, AddPoints, RuleId)),
    date_between(StartDate, EndDate, Date).

points(purchase(PersonId, ProductId, Price, Channel, Location, Campaign, Date), point(PointType, ConvRate, AddPoints, RuleId)) :-
    price_convert_rate(PointType, CChannel, CLocation, CCampaign, CProduct, CCategory, ConvRate, AddPoints, RuleId),
    point_logic(purchase(PersonId, ProductId, Price, Channel, Location, Campaign, Date),
                price_convert_rate(PointType, CChannel, CLocation, CCampaign, CProduct, CCategory, ConvRate, AddPoints, RuleId)).


get_all_points_from_purchase(Purchase, Points) :-
    findall(P, points(Purchase, P), AllPoints),
    findall(PD, points_date(Purchase, PD), AllPointsDate),
    append(AllPoints, AllPointsDate, Points).

add_point_constraint_total_time(RuleId, _, _, _) :- point_constraint_total_time(RuleId, _, _, _), fail, !.
add_point_constraint_total_time(_, *, _, *) :- fail, !.
add_point_constraint_total_time(RuleId, NeededAmount, WithinDays, NeededNumberOfPurchases) :-
    assert(point_constraint_total_time(RuleId, NeededAmount, WithinDays, NeededNumberOfPurchases)).

% -- point math --   [(default,2,200,default_rule)]

add_points(PointsList, Purchase, ExpiresDate) :-
    member(point(PointsType, PointsRate, PointsExtra, RuleId) , PointsList),
    Purchase = purchase(CustomerId, _, Price, _, _, _, _),
    Points is PointsExtra + (Price * PointsRate),
    new_points(point(CustomerId, PointsType, Points, Purchase, ExpiresDate, RuleId)),
    fail.

add_points(_,_,_) :- !.




:- begin_tests(rules).
:- use_module(rules).
:- use_module(declarations).
:- use_module(helpers).
:- use_module(defaults).

mockup :-
    reset_data,
    new_category_name(category_name(cat_top)),
    new_category_name(category_name(cat_child1)),
    new_product(product(product1)),
    new_product(product(product1)),
    new_category(category(product1, cat_child1)),
    new_subcategory(subcategory(cat_top, cat_child1)),
    new_channel(channel(web)),
    new_location(location(norway)).

clear_price_convert_rate :-
    retractall(price_convert_rate(_,_,_,_,_,_,_,_,_)),
    retractall(price_convert_rate(_,_,_,_,_,_,_,_,_,_,_)).


test(subcategories) :-
    mockup,
    in_category2(product1, cat_child1),
    in_category2(product1, cat_top).

% make a purchase and see the results from the defaults rule
test(default_purchase) :-
    mockup,
    set_defaults,
    P = purchase(petter, product1, 1000, web, norway, *, date(2019,1,1)),
    get_all_points_from_purchase(P, Points),
    [point(Point_type,Convert_rate,Add_points,Rule_id)] = Points,
    Point_type == default,
    Rule_id == default_rule,
    Add_points == 200,
    Convert_rate == 2.

% tests that a rule with date constraints
% one purchase within the time, one outside time range
test(date_purchase) :-
    clear_price_convert_rate,
    mockup,
    assert(price_convert_rate(default ,* , *, *, *, *, 2, 200, date(2018,1,1), date(2019,2,1),date_rule)),
    P = purchase(petter, product1, 1000, web, norway, *, date(2019,1,1)),
    get_all_points_from_purchase(P, [point(default,2,200,date_rule)]),
    P2 = purchase(petter, product1, 1000, web, norway, *, date(2020,1,1)),
    get_all_points_from_purchase(P2, []).

% purchased at least for 2000,- in the last 20 days
test(within_time) :-
    mockup,
    set_defaults,
    add_point_constraint_total_time(default_rule, 2000, 20, *),
    new_purchase(purchase(petter, product1, 1000, web, norway, *, date(2019,1,1))),
    P2 = purchase(petter, product1, 1000, web, norway, *, date(2019,1,10)),
    new_purchase(P2),
    get_all_points_from_purchase(P2, Points),
    Points == [point(default,2,200,default_rule)].

test(not_within_time) :-
    mockup,
    set_defaults,
    add_point_constraint_total_time(default_rule, 2000, 5, *),
    new_purchase(purchase(petter, product1, 1000, web, norway, *, date(2019,1,1))),
    P2 = purchase(petter, product1, 1000, web, norway, *, date(2019,1,10)),
    new_purchase(P2),
    get_all_points_from_purchase(P2, Points),
    Points == [].

% there must be at least three purchases
test(within_time_but_to_few_purchases) :-
    mockup,
    set_defaults,
    add_point_constraint_total_time(default_rule, 2000, 20, 3),
    new_purchase(purchase(petter, product1, 1000, web, norway, *, date(2019,1,1))),
    P2 = purchase(petter, product1, 1000, web, norway, *, date(2019,1,10)),
    new_purchase(P2),
    get_all_points_from_purchase(P2, Points),
    Points == [].



