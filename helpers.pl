:- module(helpers, [
               days_between/3, all_purchases/2, last_purchase/2, last_last_purchase/2, bigger_or_equal/2,
               now/1, add_days/3, date_between/3, purchases_days/3, add_points/5, add_price_convert_rate/9, add_price_convert_rate/11,
               purchases_within_days/3, in_category/2, in_category2/2
                ]).

:- use_module(declarations).
:- use_module(defaults).
:- use_module(library(apply)).


days_between(date(Y1, M1, D1), date(Y2, M2, D2), Days) :-
     date_time_stamp(date(Y1, M1, D1,0,0,0,0,-,-), S1),
     date_time_stamp(date(Y2, M2, D2,0,0,0,0,-,-), S2),
     Days is abs(round((S2 - S1) / (60 * 60 * 24))).

bigger_or_equal(date(Y1, M1, D1), date(Y2, M2, D2)) :-
    (   Y2 > Y1 ), ! ;
    (   Y1 == Y2, M2 > M1 ), !;
    (   Y1 == Y2, M2 == M1, D2 >= D1 ), !.

add_days(date(Y, M, D), Days, Date) :-
    NewD is D + Days,
    date_time_stamp(date(Y, M, NewD, 0, 0, 0, 0, -, -), Stamp),
    stamp_date_time(Stamp, SD, 0),
    date_time_value(date, SD, Date).

now(date(Y, M, D)) :-
    date(date(Y, M, D)).

date_between(date(Y1, M1, D1), date(Y2, M2, D2), date(Y3, M3, D3)) :-
    date_time_stamp(date(Y1, M1, D1,0,0,0,0,-,-), S1),
    date_time_stamp(date(Y2, M2, D2,0,0,0,0,-,-), S2),
    date_time_stamp(date(Y3, M3, D3,0,0,0,0,-,-), S3),
    R1 is round(S1),
    R2 is round(S2),
    R3 is round(S3),
    between(R1,R2,R3).

purchases_days_nondeterm(CustomerId, Days, Purchase) :-
    now(Now),
    NDays is Days * -1,
    add_days(Now, NDays, StartDate),
    purchase(CustomerId, A, B, C, D, E, Date),
    between(StartDate, Now, Date),
    Purchase =  purchase(CustomerId, A, B, C, D, E, Date).

purchases_days(CustomerId, Days, Purchases) :-
    findall(Purchase, purchases_days_nondeterm(CustomerId, Days, Purchase), Purchases), !.

purchase_nondeterm(CustomerId, Purchase) :-
     purchase(CustomerId, A, B, C, D, E, F),
     Purchase = purchase(CustomerId, A, B, C, D, E, F).

all_purchases(CustomerId, Purchases) :-
    findall(Purchase, purchase_nondeterm(CustomerId, Purchase), Purchases), !.

last_purchase(Purchases, P) :- last(Purchases, P).

last_last_purchase(Purchases, P) :- length(Purchases, L), L2 is L - 2, nth0(L2, Purchases, P).

% -- point math --

add_points(CustomerId, PointsType, Points, Purchase, ExpireDate) :-
    assert(point(CustomerId, PointsType, Points, Purchase, ExpireDate)).

add_price_convert_rate(PointsType, Channel, Region, Campaign, Product, Category,ConvertRate, AddPoints, RuleId) :-
    asserta(price_convert_rate(PointsType, Channel, Region, Campaign,  Product, Category, ConvertRate, AddPoints, RuleId)).

add_price_convert_rate(PointsType, Channel, Region, Campaign, Product, Category,ConvertRate, AddPoints, StartDate, EndDate, RuleId) :-
    asserta(price_convert_rate(PointsType, Channel, Region, Campaign,  Product, Category, ConvertRate, AddPoints, StartDate, EndDate, RuleId)).

% find all purchases for a person within x days

purchase_within(PersonId, StartDate, Purchase) :-
     purchase(PersonId, ProductId, Price, Channel, Location, Campaign, Date),
     Purchase = purchase(PersonId, ProductId, Price, Channel, Location, Campaign, Date),
     bigger_or_equal(StartDate, Date).

add_purchase(purchase(_PersonId, _ProductId, Price, _Channel, _Location, _Campaign, _Date), Y, Sum) :- Sum is Y + Price.

% send in zero for purchases today
purchases_within_days(PersonId, Days, (Purchases, Value)) :-
     now(Now),
     DaysNeg is Days * -1,
     add_days(Now, DaysNeg, StartDate),
     findall(Purchase,purchase_within(PersonId, StartDate, Purchase), Purchases),
     foldl(add_purchase,Purchases,0,Value).

% nondeterministic and slower
in_category(Product, Category) :- category(Product, Category).
in_category(Product, Category) :- subcategory(Category, ChildCategory), in_category(Product, ChildCategory).


% deterministic and faster
category_parent(C, C) :- !.
category_parent(LookforCategory, Category) :- subcategory(NewCategory, Category), category_parent(LookforCategory, NewCategory).
in_category2(Product, Category) :- category(Product, Category), !.
in_category2(Product, Category) :- category(Product, CategoryNew), category_parent(Category, CategoryNew).


