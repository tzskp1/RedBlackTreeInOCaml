type zero = Zero
type 'a succ = Succ of 'a
type _ nat =
  Zero : zero nat
| Succ : 'a nat -> 'a succ nat

type red
type black

type key = int

type (_,_) subtree =
  | Leaf : (black,zero nat) subtree
  | RNode : (black,'h nat) subtree * key * (black,'h nat) subtree -> (red,'h nat) subtree
  | BNode : (_,'h nat) subtree * key * (_,'h nat) subtree -> (black,'h succ nat) subtree

type 'h coloredtree =
  | Black of (black,'h) subtree
  | Red of (red,'h) subtree

type _ unbalancedtree =
  | LG : (black,'h succ nat) subtree * (red,'h nat) subtree -> 'h nat unbalancedtree
  | LL : (black,'h succ nat) subtree * (red,'h nat) subtree -> 'h nat unbalancedtree
  | GL : (black,'h succ nat) subtree * (red,'h nat) subtree -> 'h nat unbalancedtree
  | GG : (black,'h succ nat) subtree * (red,'h nat) subtree -> 'h nat unbalancedtree

type tree =
  | Root : (black,'h nat) subtree -> tree

let balance : type h. h nat unbalancedtree -> (red,h succ nat) subtree =
  function
  | LG (BNode (RNode (ll,b,_),a,g),RNode (lgl,v,lgg)) ->
     RNode (BNode (ll,b,lgl),v,BNode (lgg,a,g))
  | LL (BNode (RNode (_,b,lg),a,g),RNode (lll,v,llg)) ->
     RNode (BNode (lll,v,llg),b,BNode (lg,a,g))
  | GL (BNode (l,a,RNode (_,c,gg)),RNode (gll,v,glg)) ->
     RNode (BNode (l,a,gll),v,BNode (glg,c,gg))
  | GG (BNode (l,a,RNode (gl,c,_)),RNode (ggl,v,ggg)) ->
     RNode (BNode (l,a,gl),c,BNode (ggl,v,ggg))
  | LG (BNode (Leaf, _, _), _) -> failwith "never!"
  | LG (BNode (BNode (_, _, _), _, _), _) -> failwith "never!"
  | LL (BNode (Leaf, _, _), _) -> failwith "never!"
  | LL (BNode (BNode (_, _, _), _, _), _) -> failwith "never!"
  | GL (BNode (_, _, Leaf), _) -> failwith "never!"
  | GL (BNode (_, _, BNode (_, _, _)), _) -> failwith "never!"
  | GG (BNode (_, _, Leaf), _) -> failwith "never!"
  | GG (BNode (_, _, BNode (_, _, _)), _) -> failwith "never!"

let rec binsert : type h. key -> (black,h nat) subtree -> h nat coloredtree =
  fun q tr ->
  match tr with
  | Leaf -> Red (RNode (Leaf,q,Leaf))
  | BNode (Leaf,a,Leaf) ->
     if q < a
     then Black (BNode (RNode (Leaf,q,Leaf),a,Leaf))
     else if q > a
     then Black (BNode (Leaf,a,RNode (Leaf,q,Leaf)))
     else Black (BNode (Leaf,q,Leaf))
  | BNode (Leaf,a,RNode (Leaf,c,Leaf)) ->
     if q < a
     then Black (BNode (RNode (Leaf,q,Leaf),a,RNode(Leaf,c,Leaf)))
     else if q = a
     then Black (BNode (Leaf,q,RNode (Leaf,c,Leaf)))
     else if q < c
     then Black (BNode (RNode (Leaf,a,Leaf),q,RNode(Leaf,c,Leaf)))
     else if q = c
     then Black (BNode (Leaf,a,RNode (Leaf,q,Leaf)))
     else Black (BNode (RNode (Leaf,a,Leaf),c,RNode(Leaf,q,Leaf)))
  | BNode (RNode (Leaf,b,Leaf),a,Leaf) ->
     if q < b
     then Black (BNode (RNode (Leaf,q,Leaf),b,RNode(Leaf,a,Leaf)))
     else if q = b
     then Black (BNode (RNode (Leaf,q,Leaf),a,Leaf))
     else if q < a
     then Black (BNode (RNode (Leaf,b,Leaf),q,RNode(Leaf,a,Leaf)))
     else if q = a
     then Black (BNode (RNode (Leaf,b,Leaf),q,Leaf))
     else Black (BNode (RNode (Leaf,b,Leaf),a,RNode(Leaf,q,Leaf)))
  | BNode ((BNode _ as l),a,(RNode _ as g)) ->
     if q < a
     then binsertL q tr l
     else if q = a
     then Black (BNode (l,q,g))
     else rinsertG q tr g
  | BNode ((RNode _ as l),a,(BNode _ as g)) ->
     if q < a
     then rinsertL q tr l
     else if q = a
     then Black (BNode (l,q,g))
     else binsertG q tr g
  | BNode ((RNode _ as l),a,(RNode _ as g)) ->
     if q < a
     then rinsertL q tr l
     else if q = a
     then Black (BNode (l,q,g))
     else rinsertG q tr g
  | BNode ((BNode _ as l),a,(BNode _ as g)) ->
     if q < a
     then binsertL q tr l
     else if q = a
     then Black (BNode (l,q,g))
     else binsertG q tr g
and binsertL : type h. key -> (black,h succ nat) subtree -> (black,h nat) subtree -> h succ nat coloredtree =
  fun q (BNode (_,a,g)) l ->
  match binsert q l with
  | Red l' -> Black (BNode (l',a,g))
  | Black l' -> Black (BNode (l',a,g))
and binsertG : type h. key -> (black,h succ nat) subtree -> (black,h nat) subtree -> h succ nat coloredtree =
  fun q (BNode (l,a,_)) g ->
  match binsert q g with
  | Red g' -> Black (BNode (l,a,g'))
  | Black g' -> Black (BNode (l,a,g'))
and rinsertL : type h. key -> (black,h succ nat) subtree -> (red,h nat) subtree -> h succ nat coloredtree =
  fun q (BNode (l,a,g)) (RNode (ll,b,lg)) ->
  if q < b
  then match binsert q ll with
       | Red ll' -> Red (balance (LL (BNode (l,a,g),ll')))
       | Black ll' -> Black (BNode (RNode (ll',b,lg),a,g))
  else if q > b
  then match binsert q lg with
       | Red lg' -> Red (balance (LG (BNode (l,a,g),lg')))
       | Black lg' -> Black (BNode (RNode (ll,b,lg'),a,g))
  else Black (BNode (ll,q,lg))
and rinsertG : type h. key -> (black,h succ nat) subtree -> (red,h nat) subtree -> h succ nat coloredtree =
  fun q (BNode (l,a,g)) (RNode (gl,b,gg)) ->
  if q < b
  then match binsert q gl with
       | Red gl' -> Red (balance (GL (BNode (l,a,g),gl')))
       | Black gl' -> Black (BNode (l,a,RNode (gl',b,gg)))
  else if q > b
  then match binsert q gg with
       | Red gg' -> Red (balance (GG (BNode (l,a,g),gg')))
       | Black gg' -> Black (BNode (l,a,RNode (gl,b,gg')))
  else Black (BNode (gl,q,gg))

let insert q = fun (Root x) ->
  match binsert q x with
  | Red (RNode (l,v,g)) -> Root (BNode (l,v,g))
  | Black t -> Root t

type _ succtree =
  | Succ : (black,'h succ nat) subtree -> 'h nat succtree
  | Black : (black,'h nat) subtree -> 'h nat succtree
  | Red : (red,'h nat) subtree -> 'h nat succtree

let black_of_red = function
  | RNode (l,v,g) -> BNode (l,v,g)

let rec bremove : type h. (black,h succ nat) subtree -> key * h nat succtree =
  function
  | BNode (Leaf,v,Leaf) -> (v,Black Leaf)
  | BNode (RNode (Leaf,b,Leaf),a,Leaf) -> (b,Succ (BNode (Leaf,a,Leaf)))
  | BNode (RNode (Leaf,b,Leaf),a,RNode (Leaf,c,Leaf)) -> (b,Succ (BNode (Leaf,a,RNode (Leaf,c,Leaf))))
  | BNode (Leaf,a,RNode (Leaf,c,Leaf)) -> (a,Succ (BNode (Leaf,c,Leaf)))
  | BNode ((BNode _ as l),a,RNode (gl,b,gg)) -> bremove (BNode (RNode(l,a,gl),b,gg))
  | BNode ((BNode _ as l),a,(BNode (gl,c,gg) as g)) -> 
     begin match bremove l,gl with
     | (ml,Succ l'),_ -> (ml,Succ (BNode (l',a,g)))
     | (ml,Red l'),BNode _ -> (ml,Red (balance (LL (BNode (RNode (gl,a,gl),c,gg),l'))))
     | (ml,Red l'),Leaf -> (ml,Red (balance (LL (BNode (RNode (gl,a,gl),c,gg),l'))))
     | (ml,Red l'),RNode _ -> (ml,Red (RNode (black_of_red l',a,g)))
     | (ml,Black l'),BNode _ -> (ml,Black (BNode (RNode (l',a,gl),c,gg)))
     | (ml,Black l'),Leaf  -> (ml,Black (BNode (RNode (l',a,gl),c,gg)))
     | (ml,Black l'),RNode _ -> (ml,Red (balance (LG (BNode (RNode (l',a,l'),c,gg),gl))))
     end
  | BNode (RNode (ll,b,BNode (lgl,d,lgg)),a,g) ->
     begin match bremove ll,lgl with
     | (mll,Succ ll'),_ -> (mll,Succ (BNode (RNode (ll',b,BNode (lgl,d,lgg)),a,g)))
     | (mll,Red ll'),_ -> (mll,Succ (BNode (RNode (black_of_red ll',b,BNode (lgl,d,lgg)),a,g)))
     | (mll,Black ll'),RNode _ ->
        (mll,Succ (BNode (balance (LG (BNode (RNode (ll',b,ll'),d,lgg),lgl)),a,g)))
     | (mll,Black ll'),BNode _ ->
        (mll,Succ (BNode (BNode (RNode (ll',b,lgl),d,lgg),a,g)))
     | (mll,Black ll'),Leaf ->
        (mll,Succ (BNode (BNode (RNode (ll',b,lgl),d,lgg),a,g)))
     end

let remove =
  function
  | Root Leaf -> None
  | Root (BNode _ as tr) ->
     match bremove tr with
     | (v,Red (RNode (l,a,g))) -> Some (v,Root (BNode (l,a,g)))
     | (v,Black tr') -> Some (v,Root tr')
     | (v,Succ tr') -> Some (v,Root tr')

let empty = Root Leaf

let tree_of_list = List.fold_left (fun tr q -> insert q tr) empty

let rec list_of_tree tr =
  match remove tr with
  | None -> []
  | Some (v,tr') -> v :: list_of_tree tr'
