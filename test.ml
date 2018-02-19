module T = Redblacktree

let print_int_list ls =
  List.rev ls
  |> List.map string_of_int
  |> List.fold_left (fun res p -> p ^ " " ^ res) ""
  |> print_endline

let test orig =
  (* let () = print_string "orig: " in
   * let () = print_int_list orig in *)
  let conv = orig
             |> T.tree_of_list 
             |> T.list_of_tree in
  (* let () = print_string "conv: " in
   * let () = print_int_list conv in *)
  if (List.sort_uniq compare orig) = conv
  then print_endline "match"
  else print_endline "unmatch"

let rec range n =
  if n <= 0
  then []
  else n :: range (n - 1)

let testcase = [[1;3;6;4;5;6;];[1200;234;22341;-234;466;];[2;3543;22;25346;22;]; 
                List.rev (range 10000);
                List.rev (range 234352);
                range 10000;]

let () = List.iter test testcase
