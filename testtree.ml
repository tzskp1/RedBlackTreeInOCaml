type zero
type 'a succ = Succ of 'a
type red
type black
type key = int

type (_,_) subtree =
  | Leaf : (black,zero succ) subtree
  | RNode : (black,'h) subtree * key * (black,'h) subtree -> (red,'h) subtree
  | BNode : (_,'h) subtree * key * (_,'h) subtree -> (black,'h succ) subtree

type 'h color =
  | Black of (black,'h) subtree
  | Red of (red,'h) subtree

type tree =
  | Root : (black,_) subtree -> tree

let rec binsert : type h. key ->  (black,h) subtree -> h color =
  fun q tr ->
  match tr with
  | BNode (BNode (_, _, _), _, Leaf) -> failwith "never!"
  | BNode (Leaf, _, BNode (_, _, _)) -> failwith "never!"
  | BNode (RNode (BNode (_, _, _), _, _), _, Leaf) -> failwith "never!"
  | BNode (RNode (Leaf, _, BNode (_, _, _)), _, Leaf) -> failwith "never!"
  | BNode (Leaf, _, RNode (Leaf, _, BNode (_, _, _))) -> failwith "never!"
  | BNode (Leaf, _, RNode (BNode (_, _, _), _, _)) -> failwith "never!"
  | Leaf -> Red (RNode (Leaf,q,Leaf))
  | BNode ((BNode _ as l),a,(BNode _ as g)) ->
     if q < a
     then match binsert q l with
          | Red l' -> Black (BNode (l',a,g))
          | Black l' -> Black (BNode (l',a,g))
     else begin match binsert q g with
          | Red g' -> Black (BNode (l,a,g'))
          | Black g' -> Black (BNode (l,a,g'))
          end
  | BNode ((BNode _ as l),a,(RNode (gl,c,gg) as g)) ->
     if q < a
     then match binsert q l with
          | Red l' -> Black (BNode (l',a,g))
          | Black l' -> Black (BNode (l',a,g))
     else if q < c
     then match binsert q gl with
          | Red (RNode (gll,b,glg)) -> Black (BNode (RNode (l,a,gll),b,RNode (glg,c,gg)))
          | Black gl' -> Black (BNode (l,a,RNode (gl',c,gg)))
     else begin match binsert q gg with
          | Red (RNode (ggl,b,ggg)) -> Black (BNode (RNode (l,a,gl),c,RNode (ggl,b,ggg)))
          | Black gg' -> Black (BNode (l,a,RNode (gl,c,gg')))
          end
  | BNode ((RNode (ll,b,lg) as l),a,(BNode _ as g)) ->
     if q < b
     then match binsert q ll with
          | Red (RNode (lll,c,llg)) -> Black (BNode (RNode (lll,c,llg),b,RNode (lg,a,g)))
          | Black ll' -> Black (BNode (RNode (ll',b,lg),a,g))
     else if q < a
     then match binsert q lg with
          | Red (RNode (lgl,c,lgg)) -> Black (BNode (RNode (ll,b,lgl),c,RNode (lgg,a,g)))
          | Black lg' -> Black (BNode (RNode (ll,b,lg'),a,g))
     else begin match binsert q g with
          | Red g' -> Black (BNode (l,a,g'))
          | Black g' -> Black (BNode (l,a,g'))
          end
  | BNode (Leaf,a,Leaf) ->
     if q < a
     then Black (BNode (RNode (Leaf,q,Leaf),a,Leaf))
     else Black (BNode (Leaf,a,RNode (Leaf,q,Leaf)))
  | BNode (Leaf,a,RNode (Leaf,c,Leaf)) ->
     if q < a
     then Black (BNode (RNode (Leaf,q,Leaf),a,RNode(Leaf,c,Leaf)))
     else if q < c
     then Black (BNode (RNode (Leaf,a,Leaf),q,RNode(Leaf,c,Leaf)))
     else Black (BNode (RNode (Leaf,a,Leaf),c,RNode(Leaf,q,Leaf)))
  | BNode (RNode (Leaf,b,Leaf),a,RNode (Leaf,c,Leaf)) ->
     if q < b
     then Red (RNode (BNode (Leaf,q,Leaf),b,BNode (Leaf,a,RNode (Leaf,c,Leaf))))
     else if q < a
     then Red (RNode (BNode (Leaf,b,Leaf),q,BNode (Leaf,a,RNode (Leaf,c,Leaf))))
     else if q < c
     then Red (RNode (BNode (Leaf,b,Leaf),a,BNode (Leaf,q,RNode (Leaf,c,Leaf))))
     else Red (RNode (BNode (Leaf,b,Leaf),a,BNode (Leaf,c,RNode (Leaf,q,Leaf))))
  | BNode (RNode (Leaf,b,Leaf),a,Leaf) ->
     if q < b
     then Black (BNode (RNode (Leaf,q,Leaf),b,RNode(Leaf,a,Leaf)))
     else if q < a
     then Black (BNode (RNode (Leaf,b,Leaf),q,RNode(Leaf,a,Leaf)))
     else Black (BNode (RNode (Leaf,b,Leaf),a,RNode(Leaf,q,Leaf)))
  | BNode (RNode (ll,b,lg),a,RNode (gl,c,gg)) ->
     if q < b
     then match binsert q ll with
          | Red (RNode (lll,v,llg)) -> Red (RNode (BNode (lll,v,llg),b,BNode (lg,a,RNode (gl,c,gg))))
          | Black ll'-> Black (BNode (RNode (ll',b,lg),a,RNode (gl,c,gg)))
     else if q < a
     then match binsert q lg with
          | Red (RNode (lgl,v,lgg)) -> Red (RNode (BNode (ll,b,lgl),v,BNode (lgg,a,RNode (gl,c,gg))))
          | Black lg'-> Black (BNode (RNode (ll,b,lg'),a,RNode (gl,c,gg)))
     else if q < c
     then match binsert q gl with
          | Red (RNode (gll,v,glg)) -> Red (RNode (BNode (RNode (ll,b,lg),a,glg),v,BNode (glg,c,gg)))
          | Black gl'-> Black (BNode (RNode (ll,b,lg),a,RNode (gl',c,gg)))
     else begin match binsert q gg with
          | Red (RNode (ggl,v,ggg)) -> Red (RNode (BNode (RNode (ll,b,lg),a,gl),c,BNode (ggl,v,ggg)))
          | Black gg'-> Black (BNode (RNode (ll,b,lg),a,RNode (gl,c,gg')))
          end

let insert q = fun (Root x) ->
  match binsert q x with
  | Red (RNode (l,v,g)) -> Root (BNode (l,v,g))
  | Black t -> Root t
