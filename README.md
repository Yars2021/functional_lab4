# Лабораторная работа №4 (Суховей Ярослав P34102)

Цель: получить навыки работы со специфичными для выбранной технологии/языка программирования приёмами.

Вариант лабораторной работы назначается в зависимости от выбранной технологии. Вы можете предложить свой вариант задания, в том числе и групповой.

Варианты:

1. eDSL (embedded Domain Specific Language) для конечных автоматов. eDSL должен позволять в явном виде описывать невозможные/игнорируемые переходы между состояниями. eDSL должен быть запускаемым и генерирующим описание в формате dot (подробнее см. проект graphviz). С использованием разработанного eDSL реализовать модель (одна из, определяется вариантом задания):
    1. Лифта. При движении вниз - подбирать людей.
    2. Светофора. Перекрёсток с главной дорогой и пешеходной кнопкой.

2. eDSL для описания графов вычислительного процесса с моделью вычислений: Synchronized Data Flow. eDSL должен быть запускаемым и генерирующим описание в формате dot (подробнее см. проект graphviz). Невозможность расчёта одной из вершин (деление на ноль) не должна приводить к общему сбою. С использованием данного eDSL реализовать алгоритм расчёта корней квадратного уравнения.

    Пример графа вычислительного процесса, где на один запуск на вход подаются `a` и `b`, а в процессе вычисления получается `c` и `d`:

    ```text
            a   +------+         +------+
        ------->|      |    c    |      |   d
                |  a+b |-------->|  2/c |------->
            b   |      |         |      |
        ------->|      |         |      |
                +------+         +------+
    ```

3. Библиотека парсер комбинаторов. С разработанной библиотекой парсер комбинаторов реализовать:
    1. Парсер json.
    2. Потоковый парсер csv.

4. Библиотека для централизованного журналирования работы распределённой системы. С разработанной библиотекой реализовать: ping/pong сервер.

5. eDSL реализующий оператор `for` языка go lang на макросах.

6. eDSL для программирования чат-ботов.

7. Написать что-нибудь на F# [парсер комбинаторах](https://fsharpforfunandprofit.com/series/understanding-parser-combinators/), например свой DSL или парсер какого-нибудь не слишком сложного языка.

8. Написать [UI тесты/автоматизацию](https://lefthandedgoat.github.io/canopy/) на F# для вашего любимого сайта

9. Написать собственный [F# Type Provider](https://docs.microsoft.com/en-us/dotnet/fsharp/tutorials/type-providers/) (можно скомбинировать с парсер комбинаторами и брать типы, например, из объявлений какого-нибудь не слишком сложного языка)

10. Библиотека для работы с физическими величинами (языки со статической типизацией и multiple dispatch):

    - статическая проверка (не складывать килограммы и метры);
    - автоматический вывод типов (делим метры на минуты и получаем километры в час);
    - и т.п.

Другие примеры возможных заданий:

- система управления вычислениями на кластере (Erlang/OTP)
- система распределённого хранения данных (Erlang/OTP)
- peer2peer сервис чатов с шифрованием и хранением истории
- тактовая модель процессора и транслятор в машинный язык (Haskell)
- eDSL для разработки и анализа конечных автоматов (Haskell)
- разработка eDSL для описания разметки оконного интерфейса (Lisp)
- библиотека парсер-комбинаторов и eDSL фронтенд (Lisp)
- Web framework (Common Lisp / CLOS)
- библиотека для маршалинга данных
- разработка алгоритмов обработки данных и формальное доказательство их корректности (Coq)
- объёмная задача проекта <https://ryukzak.github.io/projects/nitta/> (Haskell)
- практически любая другая сложная и интересная задача (можете попробовать найти пересечение с другими предметами).

Общие требования:

- программа должна быть реализована в функциональном стиле;
- требуется использовать идиоматичный для технологии стиль программирования;
- задание и коллектив должны быть согласованы;
- допустима совместная работа над одним заданием.

Содержание отчёта:

- титульный лист;
- требования к разработанному ПО, включая описание алгоритма;
- реализация с минимальными комментариями;
- ввод/вывод программы;
- выводы (отзыв об использованных приёмах программирования).

## Выполнение

### Варианты реализации
- #### Стандартные процессы Erlang
    [Код системы управления](src/nodes.erl)

- #### OTP: gen_server, supervisor
    [Код супервизора](src/nodes_sup.erl)
    [Код исполнителя](src/nodes_worker.erl)

### Тестирование
Для проверки правильности параллельного исполнения был использован тест:
Каждому из вариантов реализации системы управления кластером выдавалось по 50 процессов-исполнителей и на них на 1000 случайных списков выполнялись одни и те же функции Map и Reduce.
Совпадение результатов во всех тестах говорят о том, что работают оба варианта правильно (ну или оба одинаково неправильно, что маловероятно).
```erlang
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
```

### Сравнение скорости вычисления
Для измерения времени использовался следующий код:
```erlang
-module(time_test).

-export([reset_system/2,
         respawn_workers/2,
         nodes_cluster/4,
         nodes_gen_cluster/4,
         measure_time/5,
         calculation/3,
         run_test/2]).

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
```

Выполняя ручные тесты, я обратил внимание, что время работы реализации на OTP меньше, если количество узлов кластера близко к длине списка. Если же длина списка больше, реализация на стандартных процессах выигрывает:
![image](https://github.com/Yars2021/functional_lab4/assets/79992244/874e8de5-5435-4290-b365-6b8f0f815cf3)

Чтобы это подтвердить или опровергнуть, я заупстил 100 тестов разной длины на кластерах из 50 узлов. Результат:

## Выводы
- В ходе выполнения работы я дважды реализовал простую систему управления вычислениями на кластере двумя разными способами. В первом случае я использовал стандартные процессы языка Erlang, а во втором обратился к моделям поведения gen_server и supervisor из OTP.
- Реализовывать второй вариант было проще, поскольку во-первых я до этого уже написал реализацию на стандартных процессах, что давало мне понять, как именно нужно писать код, а во-вторых модели поведения из OTP уже реализовали часть того, что мне нужно было сделать, и  позволили писать код более структурированно.
- В итоге у меня есть два варианта системы управления кластерами.
