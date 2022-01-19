include "sicstus.mzn";

predicate fzn_cost_mdd(array[int] of var int: x,    % variables constrained by COST_MDD
                       int: N,                      % number of nodes    root is node 1
		       array[int] of int: level,    % level of each node root is level 1, T is level length(x)+1
		       int: E,                      % number of edges
		       array[int] of int: from,     % edge leaving node  1..N
		       array[int] of set of int: label,    % value of variable 
		       array[int] of int: cost,      % cost of using edge
		       array[int] of int: to,       % edge entering node 0..N where 0 = T node
                       var int: totalcost           % total cost of path
		      ) =
        cost_mdd_sicstus(x, level, from, label, to, cost, totalcost);
