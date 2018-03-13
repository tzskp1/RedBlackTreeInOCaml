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

module Make (Ord: OrderedType) =
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

    type _ any_tree = Any : (_,'h nat) subtree -> 'h nat any_tree

    type _ succ_tree =
      | Succ : (black,'h succ nat) subtree -> 'h nat succ_tree
      | Black : (black,'h nat) subtree -> 'h nat succ_tree
      | Red : (red,'h nat) subtree -> 'h nat succ_tree

    type t =
      | Root : (black,'h nat) subtree -> t

    let empty = Root Leaf

    let black_of_red = function
      | RNode (l,v,g) -> BNode (l,v,g)

    let balanceLG (ll,b) a g (RNode (lgl,v,lgg)) = RNode (BNode (ll,b,lgl),v,BNode (lgg,a,g))
    let balanceLL (b,lg) a g (RNode (lll,v,llg)) = RNode (BNode (lll,v,llg),b,BNode (lg,a,g))
    let balanceGL l a (c,gg) (RNode (gll,v,glg)) = RNode (BNode (l,a,gll),v,BNode (glg,c,gg))
    let balanceGG l a (gl,c) (RNode (ggl,v,ggg)) = RNode (BNode (l,a,gl),c,BNode (ggl,v,ggg))

    let rec binsert : type h . key -> (black,h nat) subtree -> h nat any_tree =
      fun q tr ->
      match tr with
      | Leaf -> Any (RNode (Leaf,q,Leaf))
      | BNode (Leaf,a,g) when compare a q > 0 -> Any (BNode (RNode (Leaf,q,Leaf),a,g))
      | BNode (BNode _ as l,a,g) when compare a q > 0 ->
         begin match binsert q l with
         | Any l' -> Any (BNode (l',a,g))
         end 
      | BNode (RNode (ll,b,lg),a,g) when compare a q > 0 && compare b q > 0 ->
         begin match binsert q ll with
         | Any (RNode _ as ll') -> Any (balanceLL (b,lg) a g ll')
         | Any (BNode _ as ll') -> Any (BNode (RNode (ll',b,lg),a,g))
         | Any (Leaf as ll') -> Any (BNode (RNode (ll',b,lg),a,g))
         end
      | BNode (RNode (ll,b,lg),a,g) when compare a q > 0 && compare b q < 0 ->
         begin match binsert q lg with
         | Any (RNode _ as lg') -> Any (balanceLG (ll,b) a g lg')
         | Any (BNode _ as lg') -> Any (BNode (RNode (ll,b,lg'),a,g))
         | Any (Leaf as lg') -> Any (BNode (RNode (ll,b,lg'),a,g))
         end
      | BNode (RNode (ll,b,lg),a,g) when compare b q = 0 ->
         Any (BNode (RNode (ll,q,lg),a,g))
      | BNode (l,a,Leaf) when compare a q < 0 -> Any (BNode (l,a,RNode (Leaf,q,Leaf)))
      | BNode (l,a,(BNode _ as g)) when compare a q < 0 ->
         begin match binsert q g with
         | Any g' -> Any (BNode (l,a,g'))
         end 
      | BNode (l,a,RNode (gl,b,gg)) when compare a q < 0 && compare b q > 0 ->
         begin match binsert q gl with
         | Any (RNode _ as gl') -> Any (balanceGL l a (b,gg) gl')
         | Any (BNode _ as gl') -> Any (BNode (l,a,RNode (gl',b,gg)))
         | Any (Leaf as gl') -> Any (BNode (l,a,RNode (gl',b,gg)))
         end
      | BNode (l,a,RNode (gl,b,gg)) when compare a q < 0 && compare b q < 0 ->
         begin match binsert q gg with
         | Any (RNode _ as gg') -> Any (balanceGG l a (gl,b) gg')
         | Any (BNode _ as gg') -> Any (BNode (l,a,RNode (gl,b,gg')))
         | Any (Leaf as gg') -> Any (BNode (l,a,RNode (gl,b,gg')))
         end
      | BNode (l,a,RNode (gl,b,gg)) when compare b q = 0 ->
         Any (BNode (l,a,RNode (gl,q,gg)))
      | BNode (l,a,g) when compare a q = 0 -> Any (BNode (l,q,g))
      | BNode _ -> failwith "never!"

    let insert q =
      fun (Root x) ->
      match binsert q x with
      | Any (RNode (l,v,g)) -> Root (BNode (l,v,g))
      | Any (BNode _ as tr) -> Root tr
      | Any Leaf -> failwith "never!"

    let rec bremove : type h. (black,h succ nat) subtree -> key * h nat succ_tree =
      function
      | BNode (Leaf,v,Leaf) -> (v,Black Leaf)
      | BNode (RNode (Leaf,b,Leaf),a,Leaf) -> (b,Succ (BNode (Leaf,a,Leaf)))
      | BNode (RNode (Leaf,b,Leaf),a,RNode (Leaf,c,Leaf)) -> (b,Succ (BNode (Leaf,a,RNode (Leaf,c,Leaf))))
      | BNode (Leaf,a,RNode (Leaf,c,Leaf)) -> (a,Succ (BNode (Leaf,c,Leaf)))
      | BNode ((BNode _ as l),a,RNode (gl,b,gg)) -> bremove (BNode (RNode(l,a,gl),b,gg))
      | BNode ((BNode _ as l),a,(BNode (gl,c,gg) as g)) -> 
         begin match bremove l with
         | ml,Succ l' -> (ml,Succ (BNode (l',a,g)))
         | ml,Red l' -> (ml,Red (RNode (black_of_red l',a,g)))
         | ml,Black l' ->
            begin match gl with
            | RNode _ -> (ml,Red (balanceLG (l',a) c gg gl))
            | BNode _ -> (ml,Black (BNode (RNode (l',a,gl),c,gg)))
            | Leaf -> (ml,Black (BNode (RNode (l',a,gl),c,gg)))
            end
         end
      | BNode (RNode (ll,b,(BNode (lgl,d,lgg) as lg)),a,g) ->
         begin match bremove ll with
         | mll,Succ ll' -> (mll,Succ (BNode (RNode (ll',b,lg),a,g)))
         | mll,Red ll' -> (mll,Succ (BNode (RNode (black_of_red ll',b,lg),a,g)))
         | mll,Black ll' ->
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
         | v,Red (RNode (l,a,g)) -> Some (v,Root (BNode (l,a,g)))
         | v,Black tr' -> Some (v,Root tr')
         | v,Succ tr' -> Some (v,Root tr')

    let tree_of_list = List.fold_left (fun tr q -> insert q tr) empty

    let rec list_of_tree tr =
      match remove tr with
      | None -> []
      | Some (v,tr') -> v :: list_of_tree tr'
  end
