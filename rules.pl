:- module(rules, [
              get_all_points_from_purchase/2
                ]).

:- use_module(declarations).
:- use_module(helpers).
:- use_module(defaults).

points_date(purchase(_PersonId, _ProductId, _Price, Channel, Location, Campaign, Date), (PointType, ConvRate)) :-
    price_convert_rate(PointType, CChannel, CLocation, CCampaign, StartDate, EndDate, ConvRate),
    (   (CChannel == Channel ; CChannel == -) ->
    (   (CLocation == Location ; CLocation == -) ->
    (   (CCampaign == Campaign ; CCampaign == -) -> true ; fail) ; fail) ; fail),
    date_between(StartDate, EndDate, Date).

points(purchase(_PersonId, _ProductId, _Price, Channel, Location, Campaign, _Date), (PointType, ConvRate)) :-
    price_convert_rate(PointType, CChannel, CLocation, CCampaign, ConvRate),
        (   (CChannel == Channel ; CChannel == -) ->
        (   (CLocation == Location ; CLocation == -) ->
        (   (CCampaign == Campaign ; CCampaign == -) -> true ; fail) ; fail) ; fail).

get_all_points_from_purchase(Purchase, Points) :-
    findall(P, points(Purchase, P), AllPoints),
    findall(PD, points_date(Purchase, PD), AllPointsDate),
    append(AllPoints, AllPointsDate, Points).

