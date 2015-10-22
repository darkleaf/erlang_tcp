-module(chain).

-export([new/0]).
-export([can_add/2]).
-export([add/2]).
-export([to_binary/1]).

new() -> [].

can_add([], _) -> true;
can_add([LastNum | _], Num) -> Num > LastNum.

add([], Num) -> [Num];
add(Chain, Num) -> [Num | Chain].

to_binary(Chain) ->
	Strings = lists:map(fun(Num) -> integer_to_list(Num) end, Chain),
	InCorrectOrder = lists:reverse(Strings),
	String = string:join(InCorrectOrder, ","),
	list_to_binary(String).
