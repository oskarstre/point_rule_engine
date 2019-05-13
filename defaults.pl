:- module(defaults, [
               price_convert_rate/9, price_convert_rate/11
                ]).
:- dynamic price_convert_rate/9, price_convert_rate/11.

% on format point_type, channel, region, campaign, product, category,convert rate, add_points, rule_id
price_convert_rate(default ,* , *, *, *, *, 1, 100, default_rule).


% we can have convertion rates between dates like this
% price_convert_rate(point_type,channel,region,campaign,product,
% category, convert_rate, add_points,
% date(2017,1,1),date(2017,1,5),2.0), rule id



