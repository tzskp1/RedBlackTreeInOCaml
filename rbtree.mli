module type OrderedType =
  sig
    type t
    val compare : t -> t -> int
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

module Make (Ord : OrderedType) : S with type key = Ord.t
