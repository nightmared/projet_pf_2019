type ('shift) res

module GreenThreads (M: sig type shift end) : sig
	type shift
	val scheduler: (unit -> (shift) res) list -> shift -> unit

	val get: unit -> shift
	val send: shift -> unit
	val yield: unit -> unit
	val exit: unit -> unit
end
