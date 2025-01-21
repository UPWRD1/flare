with "examples/container"

type TreeNode = struct of
    left: Option<Self>,
    right: Option<Self>,
    data: Container

let make_tree of depth -> TreeNode =
    if depth > 0 then
        TreeNode {
            left: Some(make_tree(depth - 1)), 
            right: Some(make_tree(depth - 1)),
            data: Container::new(depth)
        }
    else 
        TreeNode {
            left: None,
            right: None,
            data: Container::new(depth)
        }

let main = 
    make_tree(3)