type key = string

datatype 'a tree = LEAF | TREE of 'a tree * key * 'a * 'a tree

fun insert (key, vl, LEAF) = TREE(LEAF, key, vl, LEAF)
  | insert (key, vl, TREE(l, k, cvl, r)) =
    if key < k
      then TREE(insert(key, vl, l), k, cvl, r)
    else if key > k
      then TREE(l, k, cvl, insert(key, vl, r))
    else TREE(l, k, vl, r)

fun lookup (key, LEAF) = raise Empty
  | lookup (key, TREE(l, k, cvl, r)) =
    if key < k
      then lookup(key, l)
    else if key > k
      then lookup(key, r)
    else cvl

val myTree = insert("p", 5, insert("k", 4, insert ("q", 3, insert ("g", 2, insert ("n", 1, LEAF)))))
