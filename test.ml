module T = Redblacktree

let print_int_list ls =
  List.rev ls
  |> List.map string_of_int
  |> List.fold_left (fun res p -> p ^ " " ^ res) ""
  |> print_endline

let orig = [1;3;6;4;5;6;]
let conv = orig
           |> T.tree_of_list 
           |> T.list_of_tree

let () = print_string "orig: "
let () = print_int_list orig

let () = print_string "conv: "
let () = print_int_list conv
