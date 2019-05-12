:- module(hepers, [
               days_between/3, all_purchases/2, last_purchase/2, last_last_purchase/2, bigger_or_equal/2,
               now/1, add_days/3, date_between/3, purchases_days/3, add_points/5, add_price_convert_rate/4, add_price_convert_rate/6
                ]).

:- use_module(declarations).
:- use_module(defaults).


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

add_price_convert_rate(PointsType, Channel, Region, ConvRate) :-
    asserta(price_convert_rate(PointsType, Channel, Region, ConvRate)).

add_price_convert_rate(PointsType, Channel, Region, FromDate, ToDate, ConvRate) :-
    asserta(price_convert_rate(PointsType, Channel, Region, FromDate, ToDate, ConvRate)).







