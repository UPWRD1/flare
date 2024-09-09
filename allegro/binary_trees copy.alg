with IO
with Math
with Iter

type TreeNode = struct of
    left: Option(TreeNode),
    right: Option(TreeNode),

let make_tree of depth: int -> TreeNode =
    if depth > 0 then
    TreeNode {
        left = make_tree(depth - 1), 
        right = make_tree(depth - 1)
        }
    else 
    TreeNode {
        left = None,
            right = None
        }

let check of n: TreeNode -> int =
    if is_none(n.left) then 
        return 1 
    else
     return 1 + unwrap(n.left) & check() + unwrap(n.right) & check()

let main of args  =
    min_depth = 4
    max_depth = if is_some(args[0]) then args[0] else min_depth + 2
    stretch_d = max_depth + 1
    do IO.out!("Stretch tree of depth ", stretch_d, "; check: ", make_tree(stretch_d) & check())
    long_lived_tree = make_tree(max_depth)
    mut iterations = Math.pow(2, max_depth)
    Iter.for(depth, Iter.range_step(min_depth, stretch_depth, 2), 
        fn -> 
          mut c = 0
          Iter.for(_, Iter.range(1, iterations + 1), fn ->
            c += make_tree(depth) & check()
          )
          do IO.out!(iterations, " trees of depth " depth, "; check: " check)
          iterations = (iterations / 4) & Math.floor()
    )
    do IO.out!("Long lived tree of depth: ", max_depth, "; check: " check(long_lived_tree))