-module(tests).

-include_lib("eunit/include/eunit.hrl").

nodes_cluster(MapFunc, ReduceFunc, List, Workers) ->
  MapResult = nodes:execute_map(MapFunc, List, Workers),
  nodes:execute_reduce(ReduceFunc, MapResult, Workers).

nodes_gen_cluster(MapFunc, ReduceFunc, List, Workers) ->
  MapResult = nodes_sup:execute_map(MapFunc, List, Workers),
  nodes_sup:execute_reduce(ReduceFunc, MapResult, Workers).

calculation_test_case(ListLen, Workers, WorkersGen) ->
  MapFunc = fun({X}) ->
              case (X < 0) of
                true -> -X;
                _ -> X
              end
            end,
  ReduceFunc = fun({X, Acc}) -> X + Acc end,
  List = [rand:uniform(50) || _ <- lists:seq(1, ListLen)],
  ?assertEqual(nodes_cluster(MapFunc, ReduceFunc, List, Workers),
               nodes_gen_cluster(MapFunc, ReduceFunc, List, WorkersGen)).

calculation_test() ->
  {ok, Supervisor} = nodes_sup:start_link(),
  PidsNodes = nodes:spawn_workers(50),
  PidsNodesGen = nodes_sup:spawn_workers(Supervisor, 50),
  [calculation_test_case(rand:uniform(100) - 1, PidsNodes, PidsNodesGen)
   || _ <- lists:seq(1, 1000)].
