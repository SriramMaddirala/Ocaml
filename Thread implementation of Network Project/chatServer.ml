open Lwt
(* a list associating user nicknames to the output channels that write to their connections *)
(* Once we fix the other functions this should change to []*)
let sessions = ref [("",Lwt_io.null)]
exception Quit

let mutex = Lwt_mutex.create ()

(* replace Lwt.return below with code that uses Lwt_list.iter_p to
  print sender: msg on each output channel (excluding the sender's)*)
let broadcast sender msg = Lwt_list.iter_p (fun l ->( match l with
| (a,b) -> if(a = sender) then Lwt.return () else Lwt_io.fprintl b (((sender) ^ ": ") ^ msg))) !sessions
(* remove a session from the list of sessions: important so we don't try to
   write to a closed connection *)
let remove_session nn =
  sessions := List.remove_assoc nn !sessions;
  broadcast nn "<left chat>" >>= fun () ->
  Lwt.return ()

(* Modify to handle the "Quit" case separately, closing the channels before removing the session *)
let handle_error e nn inp outp = try raise e with
| Quit-> Lwt_io.close inp >>= fun () ->  Lwt_io.close outp >>= fun () -> remove_session nn
| _ -> Lwt.return ()

(* modify sessions to remove (nn,outp) association, add (new_nn,outp) association.
   also notify other participants of new nickname *)
let change_nn nn outp new_nn = broadcast !nn ((("<changed nick to ") ^ !new_nn) ^ (">")) >>= 
fun () -> remove_session !nn; nn := !new_nn;
sessions :=  ((!new_nn,outp) :: !sessions);
Lwt.return ()

(*  + obtain initial nick(name),
    + add (nick,outp) to !sessions, and
    + announce join to other users *)
let login_handler nr (inp,outp) = Lwt_io.fprintl outp ("Enter initial nick:") 
>>= fun () -> Lwt_io.read_line inp 
>>= fun l -> nr := l; Lwt.return (Lwt_mutex.lock mutex); 
sessions :=  ((!nr,outp) :: !sessions);
Lwt.return (Lwt_mutex.unlock mutex) >>= fun () -> broadcast !nr "<joined>"

(* modify handle_input below to detect /q, /n, and /l commands *)
let handle_input nr outp l = let chck = (Str.string_before l 2) in match chck with 
| "/q" ->  raise(Quit)
| "/n" -> let strwant = ref (Str.string_after l 3) in change_nn nr outp strwant
| "/l" ->  Lwt_list.iter_s (fun l -> (match l with
                                        | (a,b) -> if(a="") then Lwt.return () else Lwt_io.fprintl outp a)) !sessions
| _ -> broadcast !nr l

let chat_handler (inp,outp) =
  let nick = ref "" in
  (* replace () below with call to login_handler *)
  let _ = login_handler nick (inp,outp) in
  let rec main_loop () =
	  Lwt_io.read_line inp >>= handle_input nick outp >>= main_loop in
  Lwt.async (fun () -> Lwt.catch main_loop (fun e -> handle_error e !nick inp outp))
