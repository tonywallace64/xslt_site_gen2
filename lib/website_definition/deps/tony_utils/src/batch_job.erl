-module(batch_job).
%% utility for executing a batch of jobs in parallel
%% receives as input a list of jobs {MFA}
%% returns that list as {{M,F,A},Result}
%% the resulting list may be in a different order
-export ([batch_submit/1,do_job/2]).

-spec batch_submit([{Module::string(),Func::string(),Args::string()}]) -> [{{Module::string(),Func::string(),Args::string()},Result::term()}].
batch_submit(Data) ->
	Number_of_jobs = length(Data),
	[submit(X) || X <- Data],
	wait_results(Number_of_jobs,[]).

wait_results(0,R) -> R;
wait_results(Num,Acc) ->
	receive
		X -> X
	end,
	wait_results(Num-1,[X|Acc]).

submit(Z={_M,_F,_A}) ->
	spawn_link(?MODULE,do_job,[self(),Z]).

do_job(MainProc,{M,F,A}) ->
	X = M:F(A),
	Result = {{M,F,A},X},
	MainProc ! Result.


