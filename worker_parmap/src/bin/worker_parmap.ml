let num_files = 10

let reader file_num =
  let name = Printf.sprintf "files/%d.dat" file_num in
  let descr = Unix.openfile name [O_RDONLY] 0o640 in
  let stream = Unix.in_channel_of_descr descr
               |> Stream.of_channel
  in
  let rec read_char stream n =
    match Stream.peek stream with
    | None -> n
    | _    ->
       Stream.junk stream;
       read_char stream (n + 1)
  in
  let nchars = read_char stream 0 in
  Unix.close descr;
  (name, nchars)

let run_readers_for num =
  Printf.printf "Starting %d workers\n" num;
  let ids = List.init num (fun i -> i + 1) in
  let results = Parmap.parmap ~ncores:num_files reader (Parmap.L ids) in
  List.iter (fun (name, n) -> Printf.printf "%s\t=> %d chars\n" name n) results

let () =
  run_readers_for num_files
