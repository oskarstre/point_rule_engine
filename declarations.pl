:- module(declarations, [new_product/1, new_campaign/1, new_point_type/1, new_location/1, new_channel/1, new_purchase/7,
                         new_category_name/1, new_category/2, new_level/3,
                         new_one_time_offer/1, new_subcategory/2,
                         purchase/7,  category_name/1, category/2, channel/1,
                         location/1, point_type/1, campaign/1, product/1, level/3,
                         one_time_offer/1, prize/3, point/5, speed/2, subcategory/2
                ]).


:- dynamic purchase/7.
:- dynamic category_name/1.
:- dynamic category/2.
:- dynamic channel/1.
:- dynamic location/1.
:- dynamic point_type/1.
:- dynamic campaign/1.
:- dynamic product/1.
:- dynamic level/3.
:- dynamic one_time_offer/1.
:- dynamic prize/3.
:- dynamic point/5.
:- dynamic speed/2.
:- dynamic subcategory/2.

% keeps track of rules which should be used only one time
new_one_time_offer(RuleId) :- one_time_offer(RuleId), !.
new_one_time_offer(RuleId) :- assert(one_time_offer(RuleId)).

% at which point number does a customer reach a new level
new_level(Level, _ , _) :- level(Level, _, _), !.
new_level(Level, PointType, Points) :- assert(level(Level, PointType, Points)).

new_product(P) :- product(P), !.
new_product(P) :- assert(product(P)).

new_campaign(C) :- campaign(C), !.
new_campaign(C) :- assert(campaign(C)).

new_point_type(P) :- point_type(P), !.
new_point_type(P) :- assert(point_type(P)).

new_location(L) :- location(L), !.
new_location(L) :- assert(location(L)).

new_channel(ChannelName) :- channel(ChannelName), !.
new_channel(ChannelName) :- assert(channel(ChannelName)).

new_purchase(PersonId, ProductId, Price, Channel, Location, Campaign, Date) :-
    product(ProductId), channel(Channel), location(Location), campaign(Campaign),
    assert(purchase(PersonId, ProductId, Price, Channel, Location, Campaign, Date)).

new_category_name(CN) :- category_name(CN), !.
new_category_name(CN) :- assert(category_name(CN)).

new_category(ProductId, CategoryName) :- category_name(CategoryName), assert(category(ProductId, CategoryName)).

new_subcategory(C1, C2) :- subcategory(C1, C2), !.
new_subcategory(C1, C2) :-
    category_name(C1),
    category_name(C2),
    assert(subcategory(C1, C2)), !.





