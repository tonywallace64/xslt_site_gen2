-module (exec_str).
-export ([string/1]).

string(Str) ->
    {ok,Ts,_} = erl_scan:string(Str),
    Ts1 = case lists:reverse(Ts) of
	      [{dot,_}|_] -> Ts;
	      TsR -> lists:reverse([{dot,1} | TsR])
	  end,
    {ok,Expr} = erl_parse:parse_exprs(Ts1),
    {value, _Value, _Bs} = erl_eval:exprs(Expr, erl_eval:new_bindings()),
    _Value.
    
