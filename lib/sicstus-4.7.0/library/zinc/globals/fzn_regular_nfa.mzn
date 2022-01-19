predicate fzn_regular_nfa(array[int] of var int: x, int: Q, int: S,
                      array[int,int] of set of int: d, int: q0, set of int: F) =
        let {
            % If x has index set m..n-1, then a[m] holds the initial state
            % (q0), and a[i+1] holds the state we're in after  processing
            % x[i].  If a[n] is in F, then we succeed (ie. accept the string).
              int: m = min(index_set(x)),
              int: n = max(index_set(x)) + 1,
              array[m..n] of var 1..Q: a,
	      array[int] of int: flat =
	           [if k=1 then sin else
                    if k=2 then j   else 
		                sout endif endif | sin in 1..Q, j in 1..S, sout in d[sin,j], k in 1..3],
              int: rows = floor(length(flat)/3),
	      array[1..rows,1..3] of int: dt = array2d(1..rows, 1..3, flat)
            } in
        a[m] = q0 /\                    % Set a[0].
        forall(i in index_set(x)) (
            table([a[i], x[i], a[i+1]], dt) % Determine a[i+1].
        ) /\
        a[n] in F                       % Check the final state is in F.
     ;
