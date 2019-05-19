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
    price_convert_rate(PointType, CChannel, CLocation, CCampaign, CProduct, CCategory, StartDate, EndDate, ConvRate),
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


