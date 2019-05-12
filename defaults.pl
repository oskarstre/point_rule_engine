:- module(defaults, [
               price_convert_rate/4, price_convert_rate/6
                ]).
:- dynamic price_convert_rate/4, price_convert_rate/6.

% on format point_type, channel, region, convert rate
price_convert_rate(- ,- ,-, 1).


% we can have convertion rates between dates like this
% price_convert_rate(point_type,channel,region,date(2017,1,1),date(2017,1,5),2.0)
