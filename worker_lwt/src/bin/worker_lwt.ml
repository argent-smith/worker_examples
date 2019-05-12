include Lwt.Infix

let num_files = 10

(* Infix AKA monadic syntax *)
let reader n =
  let name = Printf.sprintf "files/%d.dat" n in
  let stream = Lwt_io.chars_of_file name in
  let rec read_stream chars i =
    Lwt_stream.get chars
    >>=
      (function
       | None -> Lwt_io.printlf "%s\t=> %d chars" name i
       | _    -> read_stream chars (i + 1))
  in
  read_stream stream 0

(* Lwt PPX syntax *)
let reader' n =
  let name = Printf.sprintf "files/%d.dat" n in
  let stream = Lwt_io.chars_of_file name in
  let rec read_stream chars i =
    let%lwt () = Lwt_stream.junk chars in
    let%lwt () = Lwt_io.printlf "%d\t=>\t#%d\t<=\t%s" n i name in
    let%lwt () = Lwt_unix.sleep 0.001 in
    read_stream chars (i + 1) in
  read_stream stream 1

let spawn_readers_for num =
  List.init num (fun i -> i + 1)
  |> List.map reader
  |> Lwt.join

let () =
  Printf.printf "Starting %d workers\n" num_files;
  flush stdout;
  Lwt_main.run @@ spawn_readers_for num_files
