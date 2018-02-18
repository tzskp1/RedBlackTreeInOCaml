type zero
type 'a succ = Succ of 'a
type red
type black
type key = int

type (_,_) subtree =
  | Leaf : (black,zero) subtree
  | RNode : (black,'h) subtree * key * (black,'h) subtree -> (red,'h) subtree
  | BNode : (_,'h) subtree * key * (_,'h) subtree -> (black,'h succ) subtree

type 'h coloredtree =
  | Black of (black,'h) subtree
  | Red of (red,'h) subtree

type _ unbalancedtree =
  | LG : (black,'h succ) subtree * 'h coloredtree -> 'h unbalancedtree
  | LL : (black,'h succ) subtree * 'h coloredtree -> 'h unbalancedtree
  | GL : (black,'h succ) subtree * 'h coloredtree -> 'h unbalancedtree
  | GG : (black,'h succ) subtree * 'h coloredtree -> 'h unbalancedtree

type tree =
  | Root : (black,_) subtree -> tree

let balance : type h. h unbalancedtree -> h succ coloredtree =
  function
  | LG (BNode (RNode (ll,b,_),a,g),Black lg) ->
     Black (BNode (RNode (ll,b,lg),a,g))
  | LG (BNode (RNode (ll,b,_),a,g),Red (RNode (lgl,v,lgg))) ->
     Red (RNode (BNode (ll,b,lgl),v,BNode (lgg,a,g)))
  | LL (BNode (RNode (_,b,lg),a,g),Black ll) ->
     Black (BNode (RNode (ll,b,lg),a,g))
  | LL (BNode (RNode (_,b,lg),a,g),Red (RNode (lll,v,llg))) ->
     Red (RNode (BNode (lll,v,llg),b,BNode (lg,a,g)))
  | GL (BNode (l,a,RNode (_,b,gg)),Black gl) ->
     Black (BNode (l,a,RNode (gl,b,gg)))
  | GL (BNode (l,a,RNode (_,c,gg)),Red (RNode (gll,v,glg))) ->
     Red (RNode (BNode (l,a,gll),v,BNode (glg,c,gg)))
  | GG (BNode (l,a,RNode (gl,b,_)),Black gg) ->
     Black (BNode (l,a,RNode (gl,b,gg)))
  | GG (BNode (l,a,RNode (gl,c,_)),Red (RNode (ggl,v,ggg))) ->
     Red (RNode (BNode (l,a,gl),c,BNode (ggl,v,ggg)))
  | LG (BNode (Leaf, _, _), _) -> failwith "never!"
  | LG (BNode (BNode (_, _, _), _, _), _) -> failwith "never!"
  | LL (BNode (Leaf, _, _), _) -> failwith "never!"
  | LL (BNode (BNode (_, _, _), _, _), _) -> failwith "never!"
  | GL (BNode (_, _, Leaf), _) -> failwith "never!"
  | GL (BNode (_, _, BNode (_, _, _)), _) -> failwith "never!"
  | GG (BNode (_, _, Leaf), _) -> failwith "never!"
  | GG (BNode (_, _, BNode (_, _, _)), _) -> failwith "never!"

let rec binsert : type h. key -> (black,h) subtree -> h coloredtree =
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
and binsertL : type h. key -> (black,h succ) subtree -> (black,h) subtree -> h succ coloredtree =
  fun q (BNode (_,a,g)) l ->
  match binsert q l with
  | Red l' -> Black (BNode (l',a,g))
  | Black l' -> Black (BNode (l',a,g))
and binsertG : type h. key -> (black,h succ) subtree -> (black,h) subtree -> h succ coloredtree =
  fun q (BNode (l,a,_)) g ->
  match binsert q g with
  | Red g' -> Black (BNode (l,a,g'))
  | Black g' -> Black (BNode (l,a,g'))
and rinsertL : type h. key -> (black,h succ) subtree -> (red,h) subtree -> h succ coloredtree =
  fun q p (RNode (ll,b,lg)) ->
  if q < b
  then balance (LL (p,binsert q ll))
  else if q > b
  then balance (LG (p,binsert q lg))
  else Black (BNode (ll,q,lg))
and rinsertG : type h. key -> (black,h succ) subtree -> (red,h) subtree -> h succ coloredtree =
  fun q p (RNode (gl,b,gg)) ->
  if q < b
  then balance (GL (p,binsert q gl))
  else if q > b
  then balance (GG (p,binsert q gg))
  else Black (BNode (gl,q,gg))

let insert q = fun (Root x) ->
  match binsert q x with
  | Red (RNode (l,v,g)) -> Root (BNode (l,v,g))
  | Black t -> Root t

let empty = Root Leaf

;;insert 1 empty
  |> insert 2
  |> insert 3
  |> insert 4
  |> insert 5
