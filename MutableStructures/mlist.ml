type 'a ll = { mutable hd : 'a ; mutable tl : 'a mlist}
 and 'a mlist = List of 'a ll | Nil
let rec mlist_helper ls = match ls with
| hed::tel -> List {hd = hed; tl = mlist_helper tel }
| [] -> Nil
let mlist_of_list ls =  mlist_helper ls
let rec rev_mlist_helper l = match l with
| List {hd=a ; tl = List {hd=b;tl=Nil}} -> List{hd = b; tl = List{hd=a;tl=Nil}}  
| Nil -> l
let rev_mlist l = rev_mlist_helper l

let append_m l1 l2 = Nil
