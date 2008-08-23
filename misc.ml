let print_list f l =
  print_char '[';
  match l with
    [] -> ()
  | hd :: tl -> begin
    print_string (f hd);
    List.iter (fun x -> print_string (", " ^ (f x))) tl
  end;
  print_char ']';;
