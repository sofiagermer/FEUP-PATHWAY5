/** @group globals.sort
  Requires that the multiset of values in \a x are the same as the
  multiset of values in \a y but \a y is in sorted order.
*/
predicate fzn_sort(array[int] of var int: x, array[int] of var int: y);
