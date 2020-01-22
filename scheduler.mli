type 'shift res

type frequency = float option

module GreenThreads (M: sig type shift end) : sig
  val scheduler: (unit -> M.shift res) list -> M.shift -> unit

  val gen_looper: (unit -> M.shift res) -> (unit -> M.shift res)
  val gen_waiter: (unit -> unit) -> frequency -> (unit -> unit)
  val gen_scheduler_list: ((unit -> unit) * frequency) list -> (unit -> M.shift res) list 

  val get: unit -> M.shift
  val send: M.shift -> unit
  val yield: unit -> unit
  val continue: (unit -> M.shift res) -> M.shift res
  val exit: unit -> M.shift res
  val stop_scheduler: unit -> unit
end
