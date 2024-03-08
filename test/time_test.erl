-module(time_test).

-export([reset_system/2,
         respawn_workers/2,
         nodes_cluster/4,
         nodes_gen_cluster/4,
         measure_time/5,
         calculation/3,
         run_test/2,
         run_len_test/2,
         get_avg_time/2,
         test_case/3,
         test/1]).

reset_system(Workers, WorkersGen) ->
    nodes:kill_workers(Workers),
    nodes_sup:kill_workers(WorkersGen).

respawn_workers(Supervisor, N) ->
    {nodes:spawn_workers(N), nodes_sup:spawn_workers(Supervisor, N)}.

nodes_cluster(MapFunc, ReduceFunc, List, Workers) ->
    MapResult = nodes:execute_map(MapFunc, List, Workers),
    nodes:execute_reduce(ReduceFunc, MapResult, Workers).

nodes_gen_cluster(MapFunc, ReduceFunc, List, Workers) ->
    MapResult = nodes_sup:execute_map(MapFunc, List, Workers),
    nodes_sup:execute_reduce(ReduceFunc, MapResult, Workers).

measure_time(Workers, WorkersGen, FuncMap, FuncReduce, List) ->
    {timer:tc(time_test, nodes_cluster, [FuncMap, FuncReduce, List, Workers]),
     timer:tc(time_test, nodes_gen_cluster, [FuncMap, FuncReduce, List, WorkersGen])}.

calculation(ListLen, Workers, WorkersGen) ->
    MapFunc = fun({X}) ->
                case (X < 0) of
                  true -> -X;
                  _ -> X
                end
              end,
    ReduceFunc = fun({X, Acc}) -> X + Acc end,
    List = [rand:uniform(50) || _ <- lists:seq(1, ListLen)],
    time_test:measure_time(Workers, WorkersGen, MapFunc, ReduceFunc, List).

run_test(NumOfWorkers, ListLen) ->
    {ok, Supervisor} = nodes_sup:start_link(),
    {Workers, WorkersGen} = respawn_workers(Supervisor, NumOfWorkers),
    calculation(ListLen, Workers, WorkersGen).

run_len_test({Workers, WorkersGen}, 1) ->
    [calculation(1, Workers, WorkersGen)];

run_len_test({Workers, WorkersGen}, ListLen) when ListLen > 1 ->
    [calculation(ListLen, Workers, WorkersGen) |
     run_len_test({Workers, WorkersGen}, ListLen - 1)];

run_len_test({_, _}, _) -> [].

get_avg_time(List, Len) ->
    get_avg_time(List, Len, 0, 0).

get_avg_time([], Len, TimeAcc, TimeGenAcc) ->
    {TimeAcc / Len, TimeGenAcc / Len};

get_avg_time([{{Time, _}, {TimeGen, _}} | Tail], Len, TimeAcc, TimeGenAcc) ->
    get_avg_time(Tail, Len, TimeAcc + Time, TimeGenAcc + TimeGen).

test_case(_, _, 0) -> [];

test_case(Supervisor, Workers, Len) ->
    [get_avg_time(run_len_test(Workers, Len), Len) |
     test_case(Supervisor, Workers, Len - 1)].

test(MaxLen) ->
    {ok, Supervisor} = nodes_sup:start_link(),
    test_case(Supervisor, respawn_workers(Supervisor, 50), MaxLen).
