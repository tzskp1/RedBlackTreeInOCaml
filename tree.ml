type red
type black

type zero
type 'a succ = Succ of 'a
type _ nat =
  Z : zero nat
| S : 'a nat -> 'a succ nat
type (_,_) equal = Eq : ('a,'a) equal

type leaf

type ('l,'r) color =
  | LeafB of leaf
  | Red of 'l *'r
  | Black of 'l *'r

type (_,_) height =
  | LeafH : (

type key = int

type (_,_) subtree =
  | Leaf : (zero succ nat,black) subtree
  | RNode : ('b nat,black) subtree * key * ('b nat,black) subtree -> ('b nat,red) subtree
  | BNode : ('b nat,_) subtree * key * ('b nat,_) subtree -> ('b succ nat,black) subtree


type (_,_,_) tytree =
  TyLeaf : (_,_,leaf) tytree
| TyNode : 'left * 'right * 'color -> ('left,'right,'color) tytree


(* type (_,_,_,_,_) chkcolor =
 *   ChkR : (black color,black color,'a) chkcolor
 * | ChkB1 : (red color,black color,('a succ) nat) chkcolor
 * | ChkB2 : (black color,red color,('a succ) nat) chkcolor
 * | ChkB3 : (red color,red color,('a succ) nat) chkcolor *)

(* type (_,_,_) color =
 *   LLB : (leaf,leaf,black) color
 * | BBR : ((_,_,black) color,(_,_,black) color,red) color
 * | XXB : (_,_,black) color *)

(* type (_,_,_) color =
 *   R : (black,black,red) color
 * | B : (_,_,black) color *)

(* type (_,_,_,_) subtree =
 *   Leaf : (_,_,zero nat,black) subtree
 * | RNode : ((_,_,'b nat,black) subtree as 'l) * key * ((_,_,'b nat,black) subtree as 'r) -> ('l,'r,'b nat,red) subtree
 * | BNode : ((_,_,'b nat,_) subtree as 'l) * key * ((_,_,'b nat,_) subtree as 'r) -> ('l,'r,'b succ nat,black) subtree *)

(* type tree =
 *   Root : ('a,'b,'c,black) subtree -> tree
 * 
 * type (_,_,_) balance =
 *   DoneB : (((_,_,'h nat,_) subtree as 'l),((_,_,'h nat,_) subtree as 'r),('l,'r,'h succ nat,black) subtree) balance
 * | DoneR : (((_,_,'h nat,black) subtree as 'l),((_,_,'h nat,black) subtree as 'r),('l,'r,'h nat,red) subtree) balance
 * | UBalXR : (('h succ nat,'l) subtree,('h nat,('rl,'rr,red) color) subtree,('h succ succ nat,('l,('rl,'rr,black) color,black) color) subtree) balance *)

(* type (_,_,_) color =
 *   | BBR : (black,black,red) color
 *   | XXB : (_,_,black) color
 * 
 * type (_,_,_) height =
 *   | One : (_,_,zero succ nat) height
 *   | Inc : (_,_,'h nat) height * (_,_,'h nat) height -> (black,'h nat,'h succ nat) height
 *   | Stat : (black,_,'h nat) height * (black,_,'h nat) height -> (red,'h nat,'h nat) height
 * 
 * type (_,_) subtree =
 *   | Leaf : ((_,_,black) color,(black,zero nat,zero succ nat) height) subtree
 *   | Node : ((_,_,'lc) color,('lc,_,'h) height) subtree * key * ((_,_,'rc) color,('rc,_,'h) height) subtree -> (('lc,'rc,'c) color,('c,'h,'hi) height) subtree
 * 
 * type node = Nodes : ('a,'b) subtree -> node
 * 
 * let expand =
 *   List.fold_left 
 *     begin fun ls ->
 *     function 
 *     | Nodes (Node (l,_,r)) -> Nodes l :: Nodes r :: ls
 *     | Nodes Leaf -> ls
 *     end
 *     []
 * 
 * let collect i tr = 
 *   let rec iter : type c h . int -> (c,h) subtree -> node list =
 *     fun n x ->
 *     match x with
 *     | Leaf -> []
 *     | Node (l,k,r) ->
 *        if i = n
 *        then [Nodes x]
 *        else iter (n+1) l @ iter (n+1) r
 *   in iter 0 tr
 * 
 * ;;Lazy.force (lazy [1;2;3;]);;
 * 
 * ;;collect 2 (Node (Node (Node (Leaf,1,Leaf),2,Leaf),3,Node (Node (Leaf,1,Leaf),3,Leaf)));;
 * ;;expand (expand (expand (expand [Nodes (Node (Node (Node (Leaf,1,Leaf),2,Leaf),3,Node (Node (Leaf,1,Leaf),3,Leaf)))])));;
 * ;;expand [Nodes (Node (Node (Node (Leaf,1,Leaf),2,Leaf),3,Leaf))];;
 * ;;Root (Node (Node (Node (Node(Leaf,0,Leaf),1,Leaf),2,Leaf),3,Leaf));;
 * ;;Root (Node (Node (Leaf,2,Leaf),3,Leaf));;
 * 
 * let rec checkTree : type a b . (a,b) subtree -> (a * b) list =
 *   function
 *   | Leaf -> [XXB,One]
 *   | Node (l,_,r) ->
 *      begin match checkTree l,checkTree r with
 *      | 
 *      end *)

type (_,_) subtree =
  | Leaf : (zero succ nat,black) subtree
  | RNode : ('b nat,black) subtree * key * ('b nat,black) subtree -> ('b nat,red) subtree
  | BNode : ('b nat,_) subtree * key * ('b nat,_) subtree -> ('b succ nat,black) subtree

type 'a ins =
  | 

(* type 'a inc =
 *   Inc of ('a succ nat,black) subtree
 * | Stat of ('a nat,red) subtree *)

(* type tree =
 *   Root : ((_,_,black) color,'h) subtree -> tree *)

(* let value = function
 *   | RNode (_,v,_) -> v
 * 
 * let r2b = function
 *   | RNode (l,v,r) -> BNode (l,v,r) *)

type tree =
  Root : ('h nat,black) subtree -> tree

type rbtree =
  | RTree : ('h nat,red) subtree -> rbtree
  | BTree : ('h nat,black) subtree -> rbtree

let rec insert : type h . key -> rbtree -> rbtree =
  fun q tr ->
  match tr with
  | BTree Leaf -> RTree (RNode (Leaf,q,Leaf))
  | BTree (BNode ((RNode (ll,b,lg) as l),a,(RNode (gl,c,gg) as g))) ->
       if q < b
       then match insert q (BTree ll) with
            | RTree (RNode (lll,v,llg)) -> BTree (BNode (BNode (lll,v,llg),b,BNode (lg,a,g)))
            | BTree (BNode _ as ll') -> BTree (BNode (RNode (ll',b,lg),a,g))
       else if q < a
       then match insert q lg with
            | RNode (lgl,v,lgg) -> BNode (BNode (ll,b,lgl),v,BNode (lgg,a,g))
            | BNode _ as lg' -> BNode (RNode (ll,b,lg'),a,g)
       else if q < c
       then match insert q gl with
            | RNode (gll,v,glg) -> BNode (BNode (l,a,gll),v,BNode (glg,c,gg))
            | BNode _ as gl' -> BNode (l,a,RNode (gl',c,gg))
       else match insert q gg with
            | RNode (ggl,v,ggg) -> BNode (BNode (l,a,gl),c,BNode (ggl,v,ggg))
            | BNode _ as gg' -> BNode (l,a,RNode (gl,c,gg'))

(* let rec insert : type h . key -> (h nat,black) subtree -> h inc =
 *   fun q tr ->
 *   match tr with *)
  (* | BNode (l,a,g) ->
   *    if q < a
   *    then begin match l with
   *         | RNode (ll,b,lg) ->
   *            if q < b
   *            then 
   *         end *)
     (* else failwith "tori" *)
(*   | Leaf -> Stat (RNode (Leaf,q,Leaf))
 *   | BNode (Leaf,v,Leaf) -> Stat (RNode (RNode (Leaf,q,Leaf),v,Leaf))
 * 
 * type (_,_,_,_) balance =
 *   DoneB : (_,('h nat,_) subtree,('h nat,_) subtree,('h succ nat,black) subtree) balance
 * | DoneR : (_,('h nat,black) subtree,('h nat,black) subtree,('h nat,red) subtree) balance
 * | RB : (_,('h succ nat,_) subtree,('h nat,red) subtree,('h succ succ nat,black) subtree) balance
 * 
 *  ;;BNode (BNode(Leaf,1,Leaf),2,RNode(BNode(Leaf,3,Leaf),4,BNode(Leaf,5,Leaf)));;
 *  ;; Node(Leaf,1,Leaf);;
 * 
 * type (_,_,_) insert =
 *   InsL : ((zero nat,black) subtree,black,(zero succ nat,black) subtree) insert
 * | InsDn : ((('h nat,_) subtree
 *     {key:key; pp:('h0 nat,'c0) subtree; pc:('h1 nat,'c1) subtree; nc:('h1 nat,'c2) subtree; } ->
 *           (('h1 nat,'c1) subtree,('h1 nat,'c2) subtree,('h0 nat,'c0) subtree) insert
 * | InsRR : key * ('b nat,red) subtree * (_,('b succ nat,red) subtree) insert -> (('b succ nat,black) subtree,('b succ succ nat,black) subtree) insert
 * (\* | InsBB : key * ('b nat,black) subtree * (_,('b succ nat,black) subtree) insert -> (_,('b succ nat,black) subtree) insert *\)
 * 
 * let rec subtree_of_insert : type a b . (_,(a,b) subtree) insert -> (a,b) subtree =
 *   function
 *     InsL v -> BNode (Leaf,v,Leaf)
 * (\* type ('a,_) insert =
 *  *   | Ins__B : 'a * ('a,'h nat,_) subtree * ('a,('a,'h nat,_) subtree) insert -> ('a,('a,'h succ nat,black) subtree) insert
 *  *   | InsBBR : 'a * ('a,'h nat,black) subtree * ('a,('a,'h nat,black) subtree) insert -> ('a,('a,'h nat,red) subtree) insert
 *  *   | InsRBR : 'a * ('a,'h nat,red) subtree * ('a,('a,'h succ nat,black) subtree) insert -> ('a,('a,'h succ nat,red) subtree) insert
 *  *   | InsRC : 'a * ('a,'h nat,black) subtree * ('a,('a,'h succ nat,black) subtree) insert -> ('a,('a,'h succ nat,red) subtree) insert *\)
 * (\* | InsB : ('a,('a,'h nat,black) subtree,('a,'h succ nat,black) subtree) insert -> *\)
 * 
 * 
 * 
 * let insn : type a b c d . a -> (a,b nat,c) subtree -> (a,b nat,d) subtree =
 *   fun q tr ->
 *   match tr with
 *   | Leaf -> RNode (Leaf,q,Leaf)
 * 
 * let ins : type a b . a -> (a,b nat) tree -> (a,b succ nat) tree =
 *   fun q (Root tr) ->
 *   let r2b = function
 *     | RNode (l,v,r) | BNode (l,v,r) -> BNode (l,v,r)
 *   in let rec iter = function
 *        | Leaf -> RNode (Leaf,q,Leaf)
 *        | BNode (l,v,r) -> BNode (iter l,v,iter r)
 *   in Root (r2b (iter tr))
 * 
 *   (\* match tr with
 *    *      Leaf -> BNode (Leaf,q,Leaf)
 *    *    | BNode (l,v,r)
 *    *      -> BNode (ins q l,v,ins q r) *\)
 *         (\* failwith "tori" *\)
 * 
 * 
 * let rec insbn : type a b . a -> (a,b nat,_) subtree -> (a,b succ nat,_) subtree =
 *   fun q
 *   -> function
 *     BNode ((RNode(ll,lv,lr) as ltr),v,(RNode(rl,rv,rr) as rtr))
 *     -> if v > q 
 *        then BNode (RNode(Leaf,q,Leaf),v,Leaf)
 *        else if v < q 
 *        then 
 *        else BNode (Leaf,q,Leaf)
 *     (\* BNode (Leaf,v,Leaf)
 *      * -> if v > q 
 *      *    then BNode (RNode(Leaf,q,Leaf),v,Leaf)
 *      *    else if v < q 
 *      *    then BNode (Leaf,v,RNode(Leaf,q,Leaf))
 *      *    else BNode (Leaf,q,Leaf) *\)
 *   (\* | BNode (Leaf, *\)
 *   (\* | BNode (ltr,v,rtr)
 *    *   -> if v > q 
 *    *      then BNode (ins q ltr,v,rtr)
 *    *      else if v < q 
 *    *      then BNode (ltr,v,ins q rtr)
 *    *      else BNode (ltr,q,rtr) *\)
 * (\* and insrn : type a b . a -> (a,b nat,red) subtree -> (a,b succ nat,_) subtree =
 *  *   fun q
 *  *   -> function
 *  *     RNode (ltr,v,rtr)
 *  *     -> if v > q 
 *  *        then RNode (insbn q ltr,v,rtr)
 *  *        else if v < q 
 *  *        then RNode (ltr,v,insbn q rtr)
 *  *        else RNode (ltr,q,rtr) *\)
 *   (\* | Couple (Leaf l,Leaf r)
 *    *   -> if q > l 
 *    *      then if q < r
 *    *           then RNode (Leaf l,q,Leaf r)
 *    *           else RNode (leaf l,r,Leaf q)
 *    *      else RNode (leaf q,l,Leaf r) *\)
 * 
 * let insert : type a . a -> a tree -> a tree =
 *   fun q (Root tr)
 *   -> let rec iter : type a b c . a -> (a,b,c) subtree -> (a,b,c) subtree =
 *      (\* | BNode (ltr,v,rtr)
 *         *   ->  *\)
 *      in Root (iter q tr) *)
