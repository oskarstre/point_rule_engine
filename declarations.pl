:- module(declarations, [new_product/1, new_campaign/1, new_point_type/1, new_location/1, new_channel/1, new_purchase/1,
                         new_category_name/1, new_category/1, new_level/1,
                         new_one_time_offer/1, new_subcategory/1, reset_data/0, new_level_constraint/1, new_points/1, new_not_together/1, new_not_before/1,
                         purchase/8,  category_name/1, category/2, channel/1,
                         location/1, point_type/1, campaign/1, product/1, level/4,
                         one_time_offer/1, prize/3, point/6, subcategory/2, level_constraint/2, person_level/2,
                         price_convert_rate/9, price_convert_rate/11, point_constraint_total_time/4, not_together/2, not_before/2
                ]).


:- dynamic purchase/8.
:- dynamic category_name/1.
:- dynamic category/2.
:- dynamic channel/1.
:- dynamic location/1.
:- dynamic point_type/1.
:- dynamic campaign/1.
:- dynamic product/1.
:- dynamic level/4.
:- dynamic one_time_offer/1.
:- dynamic prize/3.
:- dynamic point/6.
:- dynamic subcategory/2.
:- dynamic level_constraint/2.
:- dynamic person_level/2.
:- dynamic price_convert_rate/9, price_convert_rate/11.
:- dynamic point_constraint_total_time/4.
:- dynamic not_together/2.
:- dynamic not_before/2.

% keep track of versions
% voucher
% api  
% database
% multiple categories
% multiple products
% number of products -> (qantity multiplier -> some points)
% attributes on line level only
% - order where you get points for buying two items together
% - buy two and get three
% 
% combine header rules and line rules

% add loyalty program
%
% fields: header, line items * quantity, line amounts, header amounts,
% vat
%
% order-lines and purchases
%
% UI
 

% keeps track of rules which should be used only one time
new_one_time_offer(RuleId) :- one_time_offer(RuleId), !.
new_one_time_offer(RuleId) :- assert(one_time_offer(RuleId)).

% specifies that a level must be reached for a customer, before the rule
% otherwise rule fails
new_level_constraint(level_constraint(RuleId, Level)) :-
    (   price_convert_rate(_,_,_,_,_,_,_,_,RuleId) ; price_convert_rate(_,_,_,_,_,_,_,_,_,_,RuleId)),
    level(Level, _, _, _), !,
    assert(level_constraint(RuleId, Level)).

% at which point number does a customer reach a new level
new_level(level(Level, _ , _, _)) :- level(Level, _, _, _), !.
new_level(level(Level, PointType, Points, Name)) :- assert(level(Level, PointType, Points, Name)).

new_product(product(P)) :- product(P), !.
new_product(product(P)) :- assert(product(P)).

new_campaign(campaign(C)) :- campaign(C), !.
new_campaign(campaign(C)) :- assert(campaign(C)).

new_point_type(point_type(P)) :- point_type(P), !.
new_point_type(point_type(P)) :- assert(point_type(P)).

new_location(location(L)) :- location(L), !.
new_location(location(L)) :- assert(location(L)).

new_channel(channel(ChannelName)) :- channel(ChannelName), !.
new_channel(channel(ChannelName)) :- assert(channel(ChannelName)).

new_purchase(purchase(PersonId, ProductId, Amount, Price, Channel, Location, Campaign, Date)) :-
    product(ProductId), channel(Channel), location(Location),
    (   (campaign(Campaign) ; Campaign == *) -> true ; fail) , !,
    assert(purchase(PersonId, ProductId, Amount, Price, Channel, Location, Campaign, Date)).

new_category_name(category_name(CN)) :- category_name(CN), !.
new_category_name(category_name(CN)) :- assert(category_name(CN)).

new_category(category(ProductId, CategoryName)) :- category_name(CategoryName), assert(category(ProductId, CategoryName)).

new_subcategory(subcategory(C1, C2)) :- subcategory(C1, C2), !.
new_subcategory(subcategory(C1, C2)) :-
    category_name(C1),  
    category_name(C2),
    assert(subcategory(C1, C2)), !.

new_points(point(CustomerId, PointsType, Points, Purchase, ExpireDate, RuleId)) :-
    assert(point(CustomerId, PointsType, Points, Purchase, ExpireDate, RuleId)).


add_not_togheter(M,L) :-
    assert(not_together(M, L)).

new_not_together(L) :-
    forall(member(M,L),
           add_not_togheter(M,L)
          ).

new_not_before(not_before(Old, New)) :-
    assert(not_before(Old, New)).

reset_data :-
    retractall(purchase(_,_,_,_,_,_,_)),
    retractall(category_name(_)),
    retractall(category(_,_)),
    retractall(channel(_)),
    retractall(location(_)),
    retractall(point_type(_)),
    new_point_type(point_type(default)),
    retractall(campaign(_)),
    retractall(product(_)),
    retractall(level(_,_,_,_)),
    retractall(one_time_offer(_)),
    retractall(prize(_,_,_)),
    retractall(point(_,_,_,_,_,_)),
    retractall(subcategory(_,_)),
    retractall(person_level(_,_)),
    retractall(level_constraint(_,_)),
    retractall(not_together(_,_)),
    retractall(not_before(_,_)).



