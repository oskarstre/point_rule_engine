:- module(rules, [
              get_all_points_from_purchase/2,
              price_convert_rate/9, price_convert_rate/11, point_constraint_total_time/4, add_point_constraint_total_time/4
                ]).

:- use_module(declarations).
:- use_module(helpers).

:- dynamic price_convert_rate/9, price_convert_rate/11.

:- dynamic point_constraint_total_time/4.

handle_constraint_total_time(RuleId, _) :- not(point_constraint_total_time(RuleId, _, _, _)), !.
handle_constraint_total_time(RuleId, PersonId) :-
    point_constraint_total_time(RuleId, NeededAmount, WithinDays, NeededNumberOfPurchases),
    purchases_within_days(PersonId, WithinDays, (Purchases, Value)),
    (   (NeededAmount == *; Value >= NeededAmount) -> true ; fail ),
    length(Purchases, NumPurchases),
    (   (NeededNumberOfPurchases == * ; NumPurchases >= NeededNumberOfPurchases) -> true ; fail), !.


point_logic(purchase(PersonId, Product, _Price, Channel, Location, Campaign, _Date),
            price_convert_rate(_PointType, CChannel, CLocation, CCampaign, CProduct, CCategory,_ConvRate, _AddPoints, RuleId)) :-
    (   (CCategory == * ; category(Product, CCategory)) -> true ; fail ),
    (   (CProduct == Product ; CProduct == *) -> true ; fail ),
    (   (CChannel == Channel ; CChannel == *) -> true ; fail ),
    (   (CLocation == Location ; CLocation == *) -> true ; fail),
    (   (CCampaign == Campaign ; CCampaign == *) -> true ; fail),
    handle_constraint_total_time(RuleId, PersonId).


points_date(purchase(PersonId, ProductId, Price, Channel, Location, Campaign, Date), (PointType, ConvRate, AddPoints, RuleId)) :-
    price_convert_rate(PointType, CChannel, CLocation, CCampaign, CProduct, CCategory, ConvRate, AddPoints, StartDate, EndDate, RuleId),
    point_logic(purchase(PersonId, ProductId, Price, Channel, Location, Campaign, Date),
                price_convert_rate(PointType, CChannel, CLocation, CCampaign, CProduct, CCategory, ConvRate, AddPoints, RuleId)),
    date_between(StartDate, EndDate, Date).

points(purchase(PersonId, ProductId, Price, Channel, Location, Campaign, Date), (PointType, ConvRate, AddPoints, RuleId)) :-
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

:- begin_tests(rules).
:- use_module(rules).
:- use_module(declarations).
:- use_module(helpers).
:- use_module(defaults).

mockup :-
    reset_data,
    new_category_name(cat_top),
    new_category_name(cat_child1),
    new_product(product1),
    new_product(product1),
    new_category(product1, cat_child1),
    new_subcategory(cat_top, cat_child1),
    new_channel(web),
    new_location(norway).

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
    [(Point_type,Convert_rate,Add_points,Rule_id)] = Points,
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
    get_all_points_from_purchase(P, [(default,2,200,date_rule)]),
    P2 = purchase(petter, product1, 1000, web, norway, *, date(2020,1,1)),
    get_all_points_from_purchase(P2, []).


