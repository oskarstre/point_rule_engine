:- module(rules, [
              get_all_points_from_purchase/2
                ]).

:- use_module(declarations).
:- use_module(helpers).
:- use_module(defaults).

point_logic(purchase(_PersonId, Product, _Price, Channel, Location, Campaign, _Date),
            price_convert_rate(_PointType, CChannel, CLocation, CCampaign, CProduct, CCategory,_ConvRate, _AddPoints, _RuleID)) :-
    (   (CCategory == * ; category(Product, CCategory)) -> true ; fail ),
    (   (CProduct == Product ; CProduct == *) -> true ; fail ),
    (   (CChannel == Channel ; CChannel == *) -> true ; fail ),
    (   (CLocation == Location ; CLocation == *) -> true ; fail),
    (   (CCampaign == Campaign ; CCampaign == *) -> true ; fail).


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

