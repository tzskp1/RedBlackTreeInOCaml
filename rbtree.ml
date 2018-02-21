module type OrderedType =
  sig
    type t
    val compare: t -> t -> int
  end

module type S =
  sig
    type key
    type t
    val empty: t
    val insert : key -> t -> t
    val remove : t -> (key * t) option
    val tree_of_list : key list -> t
    val list_of_tree : t -> key list
  end

module Make(Ord: OrderedType) =
  struct
    type key = Ord.t

    type zero = Zero
    type 'a succ = Succ of 'a
    type _ nat =
      | Zero : zero nat
      | Succ : 'a nat -> 'a succ nat

    type red
    type black

    type (_,_) subtree =
      | Leaf : (black,zero nat) subtree
      | RNode : (black,'h nat) subtree * key * (black,'h nat) subtree -> (red,'h nat) subtree
      | BNode : (_,'h nat) subtree * key * (_,'h nat) subtree -> (black,'h succ nat) subtree

    type t =
      | Root : (black,'h nat) subtree -> t

    let balanceLG (ll,b) a g (RNode (lgl,v,lgg)) = RNode (BNode (ll,b,lgl),v,BNode (lgg,a,g))
    let balanceLL (b,lg) a g (RNode (lll,v,llg)) = RNode (BNode (lll,v,llg),b,BNode (lg,a,g))
    let balanceGL l a (c,gg) (RNode (gll,v,glg)) = RNode (BNode (l,a,gll),v,BNode (glg,c,gg))
    let balanceGG l a (gl,c) (RNode (ggl,v,ggg)) = RNode (BNode (l,a,gl),c,BNode (ggl,v,ggg))

    type 'h uncoloredtree =
      | Black of (black,'h) subtree
      | Red of (red,'h) subtree

    let uncolor : type h. (black,h succ nat) subtree -> h nat uncoloredtree * key * h nat uncoloredtree =
      function
      | BNode ((BNode _ as l),v,(BNode _ as g)) -> (Black l,v,Black g)
      | BNode ((RNode _ as l),v,(BNode _ as g)) -> (Red l,v,Black g)
      | BNode ((RNode _ as l),v,(RNode _ as g)) -> (Red l,v,Red g)
      | BNode ((BNode _ as l),v,(RNode _ as g)) -> (Black l,v,Red g)
      | BNode ((RNode _ as l),v,(Leaf as g)) -> (Red l,v,Black g)
      | BNode ((Leaf as l),v,(Leaf as g)) -> (Black l,v,Black g)
      | BNode ((Leaf as l),v,(RNode _ as g)) -> (Black l,v,Red g)

    let bnode l q g =
      match l,g with
      | Black l',Black g' -> Black (BNode (l',q,g'))
      | Black l',Red g' -> Black (BNode (l',q,g'))
      | Red l',Black g' -> Black (BNode (l',q,g'))
      | Red l',Red g' -> Black (BNode (l',q,g'))

    let rec binsert : type h. key -> (black,h nat) subtree -> h nat uncoloredtree =
      fun q tr ->
      match tr with
      | Leaf -> Red (RNode (Leaf,q,Leaf))
      | BNode _ ->
         let l,a,g = uncolor tr in
         if compare a q > 0
         then insertL q tr l
         else if compare a q < 0
         then insertG q tr g
         else bnode l q g
    and insertL : type h. key -> (black,h succ nat) subtree -> h nat uncoloredtree -> h succ nat uncoloredtree =
      fun q (BNode (l,a,g)) ->
      function
      | Black l -> 
         begin match binsert q l with
         | Red l' -> Black (BNode (l',a,g))
         | Black l' -> Black (BNode (l',a,g))
         end
      | Red (RNode (ll,b,lg)) ->
         if compare b q > 0
         then match binsert q ll with
              | Red ll' -> Red (balanceLL (b,lg) a g ll')
              | Black ll' -> Black (BNode (RNode (ll',b,lg),a,g))
         else if compare b q < 0
         then match binsert q lg with
              | Red lg' -> Red (balanceLG (ll,b) a g lg')
              | Black lg' -> Black (BNode (RNode (ll,b,lg'),a,g))
         else Black (BNode (ll,q,lg))
    and insertG : type h. key -> (black,h succ nat) subtree -> h nat uncoloredtree -> h succ nat uncoloredtree =
      fun q (BNode (l,a,g)) ->
      function
      | Black g ->
         begin match binsert q g with
         | Red g' -> Black (BNode (l,a,g'))
         | Black g' -> Black (BNode (l,a,g'))
         end
      | Red (RNode (gl,b,gg)) ->
         if compare b q > 0
         then match binsert q gl with
              | Red gl' -> Red (balanceGL l a (b,gg) gl')
              | Black gl' -> Black (BNode (l,a,RNode (gl',b,gg)))
         else if compare b q < 0
         then match binsert q gg with
              | Red gg' -> Red (balanceGG l a (gl,b) gg')
              | Black gg' -> Black (BNode (l,a,RNode (gl,b,gg')))
         else Black (BNode (gl,q,gg))

    let insert q =
      fun (Root x) ->
      match binsert q x with
      | Red (RNode (l,v,g)) -> Root (BNode (l,v,g))
      | Black t -> Root t

    type _ succtree =
      | Succ : (black,'h succ nat) subtree -> 'h nat succtree
      | Black' : (black,'h nat) subtree -> 'h nat succtree
      | Red' : (red,'h nat) subtree -> 'h nat succtree

    let black_of_red = function
      | RNode (l,v,g) -> BNode (l,v,g)

    let rec bremove : type h. (black,h succ nat) subtree -> key * h nat succtree =
      function
      | BNode (Leaf,v,Leaf) -> (v,Black' Leaf)
      | BNode (RNode (Leaf,b,Leaf),a,Leaf) -> (b,Succ (BNode (Leaf,a,Leaf)))
      | BNode (RNode (Leaf,b,Leaf),a,RNode (Leaf,c,Leaf)) -> (b,Succ (BNode (Leaf,a,RNode (Leaf,c,Leaf))))
      | BNode (Leaf,a,RNode (Leaf,c,Leaf)) -> (a,Succ (BNode (Leaf,c,Leaf)))
      | BNode ((BNode _ as l),a,RNode (gl,b,gg)) -> bremove (BNode (RNode(l,a,gl),b,gg))
      | BNode ((BNode _ as l),a,(BNode (gl,c,gg) as g)) -> 
         begin match bremove l with
         | (ml,Succ l') -> (ml,Succ (BNode (l',a,g)))
         | (ml,Red' l') -> (ml,Red' (RNode (black_of_red l',a,g)))
         | (ml,Black' l') ->
            begin match gl with
            | RNode _ -> (ml,Red' (balanceLG (l',a) c gg gl))
            | BNode _ -> (ml,Black' (BNode (RNode (l',a,gl),c,gg)))
            | Leaf -> (ml,Black' (BNode (RNode (l',a,gl),c,gg)))
            end
         end
      | BNode (RNode (ll,b,(BNode (lgl,d,lgg) as lg)),a,g) ->
         begin match bremove ll with
         | (mll,Succ ll') -> (mll,Succ (BNode (RNode (ll',b,lg),a,g)))
         | (mll,Red' ll') -> (mll,Succ (BNode (RNode (black_of_red ll',b,lg),a,g)))
         | (mll,Black' ll') ->
            begin match lgl with
            | RNode _ -> (mll,Succ (BNode ((balanceLG (ll',b) d lgg lgl),a,g)))
            | BNode _ -> (mll,Succ (BNode (BNode (RNode (ll',b,lgl),d,lgg),a,g)))
            | Leaf -> (mll,Succ (BNode (BNode (RNode (ll',b,lgl),d,lgg),a,g)))
            end
         end

    let remove =
      function
      | Root Leaf -> None
      | Root (BNode _ as tr) ->
         match bremove tr with
         | (v,Red' (RNode (l,a,g))) -> Some (v,Root (BNode (l,a,g)))
         | (v,Black' tr') -> Some (v,Root tr')
         | (v,Succ tr') -> Some (v,Root tr')

    let empty = Root Leaf

    let tree_of_list = List.fold_left (fun tr q -> insert q tr) empty

    let rec list_of_tree tr =
      match remove tr with
      | None -> []
      | Some (v,tr') -> v :: list_of_tree tr'
  end
