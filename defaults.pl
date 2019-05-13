:- module(defaults, [
               price_convert_rate/5, price_convert_rate/7
                ]).
:- dynamic price_convert_rate/5, price_convert_rate/7.

% on format point_type, channel, region, campaign, convert rate
price_convert_rate(default ,* , *, *, 1).


% we can have convertion rates between dates like this
% price_convert_rate(point_type,channel,region,campaign,date(2017,1,1),date(2017,1,5),2.0)


