type 'a dlink = { mutable v : 'a ; mutable next : 'a dl ; mutable prev : 'a dl}
 and 'a dl = End | Link of 'a dlink
 and 'a dlist = List of 'a dl ref * 'a dl ref

let empty () = List (ref End, ref End)

let rec list_of_dl lnk = match lnk with
| End -> []
| Link n -> n.v::(list_of_dl n.next)

let rec list_of_dlist (List (f,b)) = list_of_dl !f

let rec dl_of_dlist (List (f,b)) = !f

let rec snochelper a b = match a with
| Link {v=c; next = End; prev = d} -> (Link { v = c; next = Link {v=b; next = End; prev = a}; prev =d} )
| Link {v=c; next = d; prev = e} -> Link {v=c; next = (snochelper d b); prev = e}

let snoc a b = match a with
| List (c, _) -> (match !c with 
		| End -> c := (Link {v = b; next = End; prev =End})
		| Link d -> let e = snochelper (Link d) b in c := e )
let dltodlink a = match a with
| Link x -> x

let rec remhelper a b = match a with
| Link z -> match z with 
	| {v=c; next = e; prev = d} -> if(c != b) then (match z with
						| {v= f; next = End; prev = n} -> a
						| {v=f; next = l; prev =n}-> remhelper l b)
					      else if(d!= End) then let dt = dltodlink d in let et = dltodlink e in dt.next<- e; et.prev <- d; d else End 
let remove b a = match a with
| List (c, _) ->( match !c with
                | End -> ()
                | Link d -> let e =remhelper (!c) b in c:= e)

let rec foldfun f x d = match d with
| Link {v=c; next = e; prev = d} ->  foldfun f (f x c) e
| End -> x

let fold f x lst = match lst with
| List (a, _) -> (match !a with
		| Link {v= e; next = g; prev = l} -> foldfun f x !a)

let rec foldfunRight f x d = match d with
| Link {v=c; next = e; prev = d} ->  foldfunRight f (f c x) d
| End -> x

let rec getToEnd x = match x with
| Link {v=c; next = End; prev = d} ->x
| Link {v = c; next = a; prev = d} -> getToEnd a

let reduce f lst x = match lst with
| List (a, _) -> (match !a with
                | Link {v= e; next = g; prev = l} -> foldfunRight f x (getToEnd !a))
let pop_front lst = match lst with
| List (a,_) -> (match !a with
		| Link {v = e; next = End; prev =l} -> a:= End
		| Link {v= e; next = d; prev =l} -> let dt = dltodlink d in dt.prev <- End; a:=d)
