(* this line makes the lwt bind infix operator available in scope *)
open Lwt

(* the code to handle connection ending goes here: *)

(* Here, before echo_handler, is where we'll add the input line handler: *)

let echo_handler (inp,outp) =
  let rec main_loop () =
    Lwt_io.read_line inp >>= fun l ->
    Lwt_io.write_line outp l >>= main_loop in
  Lwt.async (fun () -> Lwt.catch main_loop (fun e -> Lwt.return ()))

let s = Lwt_io.establish_server (Unix.ADDR_INET(Unix.inet_addr_any, 16384)) echo_handler
let _ = Lwt_main.run (fst (Lwt.wait ()))
