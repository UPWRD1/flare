type Option<?T> = enum of
    Some(?T),
    None

type TreeNode = struct of
    left: Option<TreeNode>,
    right: Option<TreeNode>

let make_tree of depth -> TreeNode =
    if depth > 0 then
        TreeNode {
            left = Some(make_tree(depth - 1)), 
            right = Some(make_tree(depth - 1))
        }
    else 
        TreeNode {
            left = None,
            right = None
        }

let main = 
    make_tree(3)