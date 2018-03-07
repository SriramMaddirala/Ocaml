open Lwt
let philosopher a b c = Lwt_unix.sleep (Random.float 0.001) >>= 
fun () -> Lwt_mutex.lock b >>=
fun () -> Lwt_io.printl (a ^ " got left chopstick!") >>= 
fun () -> Lwt_mutex.lock c >>= 
fun () -> Lwt_unix.sleep (Random.float 0.001) >>=
fun () -> Lwt_io.printl (a ^ " got right chopstick, eating!") >>=
fun () -> Lwt_unix.sleep (Random.float 0.001) >>=
fun () -> Lwt_io.printl (a ^ " all done eating!") >>=
fun () -> Lwt.return(Lwt_mutex.unlock c) >>=
fun () -> Lwt.return(Lwt_mutex.unlock b)

let rec philchecker a c = 
if (Lwt_mutex.is_locked c) then Lwt_io.printl (a ^ " can wait (sigh)...") >>= 
fun () -> Lwt_unix.sleep (Random.float 0.001) >>= 
fun () -> philchecker a c else Lwt.return(())
let polite_phil a b c = Lwt_unix.sleep (Random.float 0.001) >>=
fun () -> Lwt_mutex.lock b >>=
fun () -> Lwt_io.printl (a ^ " got left chopstick!") >>=
fun () -> Lwt_mutex.lock c >>= 
fun () -> Lwt_unix.sleep (Random.float 0.001) >>=
fun () -> philchecker a c >>=
fun () -> Lwt_io.printl (a ^ " got right chopstick, eating!") >>=
fun () -> Lwt_unix.sleep (Random.float 0.001) >>=
fun () -> Lwt_io.printl (a ^ " all done eating!") >>=
fun () -> Lwt.return(Lwt_mutex.unlock c) >>=
fun () -> Lwt.return(Lwt_mutex.unlock b)
let slp () = Lwt_unix.sleep (Random.float 0.001)
