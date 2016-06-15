-module (cartesian).

-export([product/1]).

product([HeadList|RemainingLists]) ->
  [[X|Y] || X <- HeadList, Y <- product(RemainingLists)];
product([]) -> [[]].
