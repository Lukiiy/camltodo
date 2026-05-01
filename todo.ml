let tasks = ref []

let () =
  print_endline "";

  let rec loop () = (* funny loop *)
    (match !tasks with
      | [] -> print_endline "-"
      | _ -> List.iteri (fun index text -> Printf.printf "%d. %s\n" (index + 1) text) !tasks
    );

    print_endline "Subcommands: add [task], mark [index], quit";
    print_string "> ";

    flush stdout; (* keep prompt visible before input *)

    let line = (* get input *)
      try read_line () with End_of_file -> "quit"
    in

    let args = String.split_on_char ' ' line in

    (match args with (* Check subcommand *)
      | "add" :: rest ->
        let text = String.concat " " rest |> String.trim in

        if text <> "" then
          tasks := !tasks @ [text]
        else
          print_endline "Usage: add [task]"

      | [ "mark"; index ] ->
        (try
          let id = int_of_string index in

          tasks := List.filteri (fun idx _ -> idx <> id - 1) !tasks (* remove task by filtering it out *)

          with _ -> print_endline "Invalid id"
        )

      | [ "quit" ] -> exit 0
      | _ -> print_endline "Unknown command"
    );

    print_endline "";
    loop ()
  in

  loop ()