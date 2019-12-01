val taxi_distance: int -> int -> int -> int -> int

module type LocationType =
  sig
    type t
    val guess_distance : t -> t -> int
    val neighbors : t -> (t * int) list
    val compare : t -> t -> int
    val equal : t -> t -> bool
    val hash : t -> int
  end
module Make :
  functor (Location : LocationType) ->
  sig
    val distance : Location.t -> Location.t -> int
    val path : Location.t -> Location.t -> Location.t list
    val find: Location.t -> Location.t -> Location.t list * int
  end
