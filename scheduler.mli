type 'shift res

module GreenThreads (M: sig type shift end) : sig
	val scheduler: (unit -> M.shift res) list -> M.shift -> unit

	val get: unit -> M.shift
	val send: M.shift -> unit
	val yield: unit -> unit
	val exit: unit -> M.shift res
	val stop_scheduler: unit -> unit
end
