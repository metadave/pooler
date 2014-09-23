-module(proper_tests).

-export( [initial_state/0,
          command/1,
          precondition/2,
          postcondition/3,
          next_state/3]).

-include_lib("proper/include/proper.hrl").
-include_lib("eunit/include/eunit.hrl").
-include("../src/pooler.hrl").


-record(state, {worker_ids=[] }).

initial_state() ->
    #state{}.


gen_worker_id(S) ->
    elements(S#state.worker_ids).



command(#state{worker_ids=[]}) ->
    ok;
command(S) ->
    oneof([
                {call, pooler, take_member, [pool1]},
                {call, pooler, return_member, [pool1, gen_worker_id(S), ok]}
            ]).

precondition(_S, {call, pooler, take_member, [pool1]}) ->
    true;
precondition(_S, {call, pooler, return_member, [pool1, _WorkerId, _WorkerState]}) ->
    true.


postcondition(_S, {call, pooler, take_member, [pool1]}, Res) ->
    io:format(user, "TAKE MEMBER -> ~p~n",[Res]),
    true;
postcondition(_S, {call, pooler, return_member, [pool1, WorkerId, WorkerState]}, Res) ->
    io:format(user, "Info = ~p ~p ~p~n", [WorkerId, WorkerState, Res]),
    true.

next_state(S, _V, {call, _, _, _}) ->
    S.

prop_pooler() ->

    Pools = 
       [ 
        [{name, pool1},
            {max_count, 5},
            {init_count, 2},
            {start_mfa,
            {pooled_gs, start_link, [{"p1"}]}}],

        [{name, pool2},
            {max_count, 5},
            {init_count, 2},
            {start_mfa,
            {pooled_gs, start_link, [{"p2"}]}}]
            ],
    application:set_env(pooler, pools, Pools),
    io:format(user, "Starting pooler statem~n", []),
    ?FORALL(Cmds, commands(?MODULE, initial_state()),
                begin
                    {_Seq,_P,ok} = run_commands(?MODULE, Cmds)
                end),
    true.

proper_test() ->
    ?assertEqual(true, proper:quickcheck(prop_pooler(), [{to_file, user}])).

