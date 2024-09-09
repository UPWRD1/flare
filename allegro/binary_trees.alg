type Option(T) = enum of
    Some(T)
    None,
end

type TreeNode = struct of
    left: Option(TreeNode),
    right: Option(TreeNode),
end

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