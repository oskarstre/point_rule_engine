% Create dummy knowledge base

:- use_module(declarations).

% one time rules

 
:-	new_one_time_offer(rule_one_t_1).
:- 	new_one_time_offer(rule_one_t_2).
:- 	new_one_time_offer(rule_one_t_3). 

 
 
	% Level specific rules
%:-	new_level_constraint(level_constraint(rule1, silver)).
%:-	new_level_constraint(level_constraint(rule2, gold)).
%:-	new_level_constraint(level_constraint(rule__one_t_1, bronze)).
  
	% Levels
:-	new_level(level(0, simple, 0,   bronze)).
:-	new_level(level(1, simple, 100, silver)).
:-	new_level(level(2, simple, 200, gold)).

 
	
	% Products
:-	new_product(product(basic_socks)).
:-	new_product(product(blue_socks)).
:-	new_product(product(red_socks)).


	% Campaigns
:-	new_campaign(campaign(sock_campaign)).
:-	new_campaign(campaign(regular_campaign)).
	 
	% Point types
:-	new_point_type(point_type(simple)).
	
	% Locations
:-	new_location(location(riga)).
	
	% Channels
:-	new_channel(channel(simple_channel)).
	
		
	% Purchases
:-	new_purchase(purchase(1, blue_socks, 100, 1, simple_channel, riga, sock_campaign,  date(2019, 1, 1))).
:-	new_purchase(purchase(1, red_socks,  500, 1, simple_channel, riga, sock_campaign,  date(2019, 1, 1))).
:-	new_purchase(purchase(1, basic_socks, 10, 1, simple_channel, riga, regular_campaign, date(2019, 3, 1))).
:-	new_purchase(purchase(2, basic_socks, 10, 1, simple_channel, riga, regular_campaign, date(2019, 1, 12))).
:-	new_purchase(purchase(3, red_socks,  500, 1, simple_channel, riga, sock_campaign,  date(2019, 1, 12))).
:-	new_purchase(purchase(2, basic_socks, 10, 1, simple_channel, riga, sock_campaign,  date(2019, 3, 1))).
	
	% Category names
:-	new_category_name(category_name(apparel)).
:-	new_category_name(category_name(socks)).
	
	% Categories
:-	new_category(category(basic_socks, socks)).
:-	new_category(category(blue_socks,  socks)).
:-	new_category(category(red_socks,   socks)). 
	
	% Subcategories
:-	new_subcategory(subcategory(apparel, socks)).
	
	% Dummy points
:-	new_points(point(1, simple, 1000, a, date(2019, 12 ,30), 1)).
:-	new_points(point(1, simple, 5000, a, date(2019, 12 ,30), 1)).
:-	new_points(point(1, simple, 100,  a, date(2019, 12 ,30), 2)).
:-	new_points(point(2, simple, 100,  a, date(2019, 12 ,30), 2)).
:-	new_points(point(3, simple, 5000, a, date(2019, 12 ,30), 1)).
:-	new_points(point(2, simple, 100,  a, date(2019, 12 ,30), 1)).

%test