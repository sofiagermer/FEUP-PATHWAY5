include "sicstus.mzn";

/**  @group globals.alldifferent
  Constrain the array of integers \a vs to be all different except those
  elements that are assigned the value 0.
*/
predicate fzn_alldifferent_except_0(array[int] of var int: vs) =
    if length(vs)<2 then true
    else
    let {
      int: u = ub_array(vs);
    } in
    if u != infinity then
      alldiff_0_sicstus(vs, u)
    else
      forall(i, j in index_set(vs) where i < j) (
         (vs[i] != 0 /\ vs[j] != 0) -> vs[i] != vs[j]
      )
    endif
    endif;
