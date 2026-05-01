type task = {
  id: int;
  text: string;
}

let tasks = ref []

let () =
  let rec loop () = (* funny loop *)
    (match !tasks with
      | [] -> print_endline "-"
      | _ -> List.iter (fun task -> Printf.printf "%d. %s\n" task.id task.text) !tasks
    );

    print_endline " ";
    print_endline "Subcommands: add [task], mark [id], quit";
    print_string "> ";

    flush stdout; (* keep prompt visible before input *)

    let line = (* get input *)
      try read_line () with End_of_file -> "quit"
    in

    let args = String.split_on_char ' ' line in

    (match args with
      | "add" :: rest ->
        let text = String.concat " " rest |> String.trim in

        if text <> "" then
          let nextId =
            List.fold_left (fun acc task -> max acc task.id) 0 !tasks + 1
          in

          tasks := !tasks @ [{
            id = nextId;
            text = text;
          }]
        else
          print_endline "Usage: add [task]"

      | [ "mark"; taskId ] ->
        (try
          let id = int_of_string taskId in

          tasks := List.filter (fun t -> t.id <> id) !tasks

          with _ -> print_endline "Invalid id"
        )

      | [ "quit" ] -> exit 0
      | _ -> print_endline "Unknown command"
    );

    print_endline "";
    loop ()
  in

  loop ()