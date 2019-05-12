let num_files = 10

let reader file_num =
  let name = Printf.sprintf "files/%d.dat" file_num in
  let stream = Lwt_io.chars_of_file name in
  let rec read_char stream n =
    match%lwt Lwt_stream.get stream with
    | None -> Lwt.return (name, n)
    | _    -> read_char stream (n + 1)
  in
  read_char stream 0

let collect_results =
  let rec append_to_ready ready pending =
    let%lwt (ready', pending') = pending in
    let ready'' = List.append ready ready' in
    match List.length pending' with
    | 0 -> Lwt.return ready''
    | _ -> Lwt.nchoose_split pending'
           |> append_to_ready ready''
  in
  append_to_ready []

let run_readers_for num =
  let%lwt () = Lwt_io.printlf "Starting %d workers" num in
  List.init num (fun i -> i + 1)
  |> List.map reader
  |> Lwt.nchoose_split
  |> collect_results

let () =
  run_readers_for num_files
  |> Lwt_main.run
  |> List.iter (fun (name, nchars) -> Printf.printf "%s\t=> %d chars\n" name nchars)
