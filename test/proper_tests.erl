-module(proper_tests).

-export( [initial_state/0,
          command/1,
          precondition/2,
          postcondition/3,
          next_state/3]).

-include_lib("proper/include/proper.hrl").
-include_lib("eunit/include/eunit.hrl").
-include("../src/pooler.hrl").
-compile([export_all]).

-behaviour(proper_statem).


-record(state, {worker_ids=[]}).

initial_state() ->
    #state{}.


gen_worker_id(S) ->
    elements(S#state.worker_ids).

command(#state{worker_ids=[]}) ->
   {call, pooler, take_member, [pool1]};
command(S) ->
    oneof([
                {call, pooler, take_member, [pool1]},
                {call, pooler, return_member, [pool1, gen_worker_id(S), ok]}
            ]).

precondition(_S, {call, pooler, take_member, [pool1]}) ->
    true;
precondition(S, {call, pooler, return_member, [pool1, WorkerId, _WorkerState]}) ->
    lists:member(WorkerId, S#state.worker_ids).

postcondition(_S, {call, pooler, take_member, [pool1]}, _Res) ->
    true;
postcondition(_S, {call, pooler, return_member, [pool1, _WorkerId,
                                                 _WorkerState]}, _Res) ->
    true.

next_state(S, V, {call, pooler, take_member, _}) ->
    S#state{worker_ids = [V|S#state.worker_ids]};

next_state(S, _V, {call, pooler, return_member, [_, WorkerId, _]}) ->
    S#state{worker_ids = lists:delete(WorkerId, S#state.worker_ids)}.

prop_pooler() ->
    ?FORALL(Cmds, commands(?MODULE, initial_state()),
                begin
                    % this is NOT correct
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
                    application:start(pooler),
                    {_Seq,_P,ok} = run_commands(?MODULE, Cmds),
                    true
                end).

proper_test() ->
    io:format(user, "Starting pooler statem~n", []),
    proper:quickcheck(prop_pooler(), [{to_file, user}]),
    io:format(user, "pooler statem finished~n", []),
    true.


