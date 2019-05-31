:- module(defaults, [
              set_defaults/0
                ]).
:- use_module(rules).

% on format point_type, channel, region, campaign, product, category,convert rate, add_points, rule_id
set_defaults :-
    retractall(price_convert_rate(_,_,_,_,_,_,_,_,_)),
    retractall(price_convert_rate(_,_,_,_,_,_,_,_,_,_,_)),
    retractall(point_constraint_total_time(_, _, _, _)),
    assert(price_convert_rate(default ,* , *, *, *, *, 2, 200, default_rule)).


% we can have convertion rates between dates like this
% price_convert_rate(point_type,channel,region,campaign,product,
% category, convert_rate, add_points,
% date(2017,1,1),date(2017,1,5),2.0), rule id



