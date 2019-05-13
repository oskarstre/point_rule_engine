:- module(rules, [
              get_all_points_from_purchase/2
                ]).

:- use_module(declarations).
:- use_module(helpers).
:- use_module(defaults).

point_logic(purchase(_PersonId, _ProductId, _Price, Channel, Location, Campaign, _Date), price_convert_rate(_PointType, CChannel, CLocation, CCampaign, _ConvRate)) :-
    (   (CChannel == Channel ; CChannel == *) -> true ; fail ),
    (   (CLocation == Location ; CLocation == *) -> true ; fail),
    (   (CCampaign == Campaign ; CCampaign == *) -> true ; fail).


points_date(purchase(PersonId, ProductId, Price, Channel, Location, Campaign, Date), (PointType, ConvRate)) :-
    price_convert_rate(PointType, CChannel, CLocation, CCampaign, StartDate, EndDate, ConvRate),
    point_logic(purchase(PersonId, ProductId, Price, Channel, Location, Campaign, Date),  price_convert_rate(PointType, CChannel, CLocation, CCampaign, ConvRate)),
    date_between(StartDate, EndDate, Date).

points(purchase(PersonId, ProductId, Price, Channel, Location, Campaign, Date), (PointType, ConvRate)) :-
    price_convert_rate(PointType, CChannel, CLocation, CCampaign, ConvRate),
    point_logic(purchase(PersonId, ProductId, Price, Channel, Location, Campaign, Date),  price_convert_rate(PointType, CChannel, CLocation, CCampaign, ConvRate)).

get_all_points_from_purchase(Purchase, Points) :-
    findall(P, points(Purchase, P), AllPoints),
    findall(PD, points_date(Purchase, PD), AllPointsDate),
    append(AllPoints, AllPointsDate, Points).

